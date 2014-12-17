{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Main where

import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad       (when, foldM)

import           Control.Monad.Trans        (lift, liftIO)
import           Control.Monad.Trans.Either (EitherT, left, right, hoistEither)
import           Control.Monad.Reader       (ask)
import           Control.Monad.State        (get, put, modify)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString       as BS

import           Data.Monoid ((<>))
import           Data.Maybe (catMaybes)

import qualified Data.Text as T
import qualified Data.Text.Read as TR

import           System.Exit (exitFailure)

import           Data.HBCI.HbciDef
import           Data.HBCI.Types
import qualified Data.Map as M


import           Data.Time.Format (FormatTime (..), formatTime)
import           Data.Time.LocalTime (getZonedTime, ZonedTime(..))
import           System.Locale (defaultTimeLocale)


import qualified Data.HBCI.Network as NW
import           Data.HBCI.Messages
import           Data.HBCI.Gen
import           Data.HBCI.Parser
import           Data.HBCI.Constants

class Job x where
  type JobResult x :: *

  getParams :: x -> HbciReader MSGEntry

  getResult :: x -> [(T.Text, DEValue)] -> Hbci (JobResult x)


data GetBalance =
  GetBalance { gbAccountNumber :: T.Text
             }

data GetBalanceResult =
  GetBalanceResult { gbrBookedBalance  :: Int
                   , gbrCurrentBalance :: Maybe Int
                   , gbrOverdraftLimit :: Maybe Int
                   }

instance Job GetBalance where
  type JobResult GetBalance = GetBalanceResult

  getParams (GetBalance accNum) = do
    -- FIXME: Probably need to get the version of the supported saldo method from the BPD
    blz <- hbciInfoBlz <$> askInfo
    return $! M.fromList [("Saldo5", M.fromList [("KTV", DEGentry $ M.fromList [("number", DEStr accNum), ("KIK.country", DEStr "280"), ("KIK.blz", DEStr blz)])
                                                ,("allaccounts", DEentry (DEStr "N"))
                                                ])
                         ]

  getResult _job vals = do
    bal <- fromMaybe (HbciErrorInternal "Saldo5: Didn't find expected field 'booked.BTG.value'") bookedBal
    return $! GetBalanceResult bal currentBal overdraft
    where
      -- FIXME: Could also lookup currency, account number to check
      -- it's actually for the correct account, etc...
      bookedBal  = btgToInt =<< lookup "SaldoRes5.booked.BTG.value" vals
      currentBal = btgToInt =<< lookup "SaldoRes5.pending.BTG.value" vals -- FIXME: Is this correct?
      overdraft  = btgToInt =<< lookup "SaldoRes5.kredit.value" vals


sendHbciJobs :: Job x => x -> HbciIO (JobResult x)
sendHbciJobs jobs = do
  sendSync
  get >>= liftIO . putStrLn . show
  sendDialogInit
  res <- sendJobsInternal jobs
  sendDialogEnd
  return res


fromMaybe :: Monad m => HbciError -> Maybe a -> EitherT HbciError m a
fromMaybe e m = maybe (left e) right m

fromEither :: Monad m => Either T.Text a -> EitherT HbciError m a
fromEither = hoistEither . first HbciErrorOther

first :: (a -> b) -> Either a c -> Either b c
first l (Left  x)  = Left (l x)
first _ (Right x)  = Right x

deToTxt :: DEValue -> Maybe T.Text
deToTxt (DEStr x) = Just x
deToTxt _         = Nothing

readInt :: T.Text -> Maybe Int
readInt x = case TR.decimal x of
  Right (i, _) -> Just i
  _ -> Nothing

deToInt :: DEValue -> Maybe Int
deToInt x = deToTxt x >>= readInt

btgToInt :: DEValue -> Maybe Int
btgToInt x = do
  txt <- deToTxt x
  case TR.decimal txt of
    Right (euros, rest) -> case ("," `T.isPrefixOf` rest, TR.decimal $ T.drop 1 rest) of
      (True, Right (cents, _)) -> Just $ euros * 100 + cents
      _ -> Nothing
    _ -> Nothing

updateBPD :: [(T.Text, DEValue)] -> Maybe BPD
updateBPD stuff = BPD <$> (deToTxt =<< lookup "BPA.version" stuff)
                      <*> (deToInt =<< lookup "BPA.numgva" stuff)
                      <*> (deToInt =<< lookup "BPA.maxmsgsize" stuff)
                      <*> Just (M.fromList stuff)

-- FIXME: Key is not right
updateUPD :: [(T.Text, DEValue)] -> Maybe UPD
updateUPD stuff = UPD <$> (deToTxt =<< lookup "UPA.version" stuff)

sign :: FormatTime t => t -> MSGEntry -> HbciReader MSGEntry
sign localTime msg = do -- sysid tanModes (MkHbciUserInfo userID pin blz) msg =
  HbciState _ _ _ _ sysid tanModes <- askState
  HbciInfo _ _ userId pin blz <- askInfo

  fromEither $! foldM (\acc (k,v) -> nestedInsert k (DEStr v) acc) msg
    [(["SigHead", "secfunc"], head $ map tanModeSecfunc tanModes ++ [secfunc_sig_pt_1step])
    ,(["SigHead", "seccheckref"], "1234567890") -- Random number which must ocurr both in head and tail
    ,(["SigHead", "range"], "1") -- This must be 1 apparently
    ,(["SigHead", "role"], "1") -- 1, 3 or 4. 1: Issuer, 3: Cosigned, 4: Witness. Probably always 1.
    ,(["SigHead", "SecIdnDetails", "func"], "1") -- 1 for User messages, 2 for 'XyzRes'
     -- Something binary, can be empty, we'll leave this out for now.
     -- Hbci4Java sets this for DDV and RDH passports, I think to a random number, can be left out for now
     -- ,(["SigHead", "SecIdnDetails", "cid"], "0")
    ,(["SigHead", "SecIdnDetails", "sysid"], maybe "0" id sysid) -- must be obtained with a sync call -- maybe hbci does this before

     -- This is the sigid. Apparently Hbci4Java increments this every time we create a signature.
     -- Not sure if this is persisted accross requests. I think it is not.
    ,(["SigHead", "secref"], "1") -- Number to uniquify messages. Hbci4Java calls this the 'sigid', always uses the same sigid for PinTan. Let's try 1 here

    ,(["SigHead", "SecTimestamp", "date"], T.pack $ formatTime defaultTimeLocale "%Y%m%d" localTime) -- YYYYMMDD
    ,(["SigHead", "SecTimestamp", "time"], T.pack $ formatTime defaultTimeLocale "%H%M%S" localTime) -- HHmmss

    ,(["SigHead", "HashAlg", "alg"], "999")

    ,(["SigHead", "SigAlg", "alg"], "10")
    ,(["SigHead", "SigAlg", "mode"], sigmode_iso9796_1)

    ,(["SigHead", "KeyName", "userid"], userId)
    ,(["SigHead", "KeyName", "country"], "280")
    ,(["SigHead", "KeyName", "blz"], blz)
    ,(["SigHead", "KeyName", "keynum"], "0") -- It's what hbci4java does for PinTan ...
    ,(["SigHead", "KeyName", "keyversion"], "0") -- It's what hbci4java does for PinTan ...

    ,(["SigTail", "UserSig", "pin"], pin)
    ,(["SigTail", "seccheckref"], "1234567890")
    ]

-- FIXME: Make at least blz typesafe
askMsgDef :: T.Text -> HbciReader MSG
askMsgDef msgName = do
  HbciInfo _ hbciDef _ _ _ <- askInfo
  fromMaybe (HbciErrorInternal $ "Can't find definition of " <> msgName) $ M.lookup msgName hbciDef

crypt :: FormatTime t => t -> MSG -> MSGEntry -> HbciReader MSGValue
crypt localTime (MSG reqSig reqEnc items) entries = do
  cryptMsgDef <- askMsgDef "Crypted"
  HbciState _ _ dialogId msgNum sysId _ <- askState
  HbciInfo _ _ userId _ blz <- askInfo

  fromEither $ finalizeMsg $ do
    items' <- let n = length items
          in if n >= 2
             then return $ take (n-2) $ drop 1 items
             else lift $ Left $ FillError [] "crypt: Need MsgHead and MsgTail in items"

    put (MkFillState 0 2)
    msgtext <- gen <$> fillMsg entries (MSG reqSig reqEnc items')

    cryptItems <- case (foldM (\acc (k,v) -> nestedInsert k v acc) M.empty
                        [(["CryptHead", "secfunc"] , DEStr "998") -- FIXME
                        ,(["CryptHead", "role"], DEStr "1") -- FIXME

                        ,(["CryptHead", "SecIdnDetails", "func"], DEStr "1") -- "2" for Responses
                        ,(["CryptHead", "SecIdnDetails", "sysid"], DEStr $ maybe "0" id sysId)
                         -- ,(["CryptHead", "SecIdnDetails", "cid"], DEStr "") for DDV

                        ,(["CryptHead", "SecTimestamp", "date"], DEStr $ T.pack $ formatTime defaultTimeLocale "%Y%m%d" localTime)
                        ,(["CryptHead", "SecTimestamp", "time"], DEStr $ T.pack $ formatTime defaultTimeLocale "%H%M%S" localTime)

                        ,(["CryptHead", "CryptAlg", "mode"], DEStr "2") -- FIXME
                        ,(["CryptHead", "CryptAlg", "alg"], DEStr "13") -- FIXME
                        ,(["CryptHead", "CryptAlg", "enckey"], DEBinary "\0\0\0\0\0\0\0\0")
                        ,(["CryptHead", "CryptAlg", "keytype"], DEStr "5") -- FIXME

                        ,(["CryptHead", "KeyName", "country"], DEStr "280")
                        ,(["CryptHead", "KeyName", "blz"], DEStr blz)
                        ,(["CryptHead", "KeyName", "userid"], DEStr userId)
                        ,(["CryptHead", "KeyName", "keynum"] ,DEStr "0") -- FIXME
                        ,(["CryptHead", "KeyName", "keyversion"], DEStr "0") -- FIXME

                        ,(["CryptHead", "compfunc"], DEStr "0") -- FIXME

                        ,(["CryptHead", "SecProfile", "method"], DEStr "1") -- FIXME
                        ,(["CryptHead", "SecProfile", "version"], DEStr "1") -- FIXME

                        ,(["CryptData","data"], DEBinary msgtext)
                        -- FIXME: why do we need this? I guess it should be automated when filling
                        ,(["MsgHead", "dialogid"], DEStr dialogId)
                        ,(["MsgHead", "msgnum"], DEStr $ T.pack $ show msgNum)
                        ,(["MsgTail", "msgnum"], DEStr $ T.pack $ show msgNum)
                        ]
                       ) of
                    Left txt -> lift $ Left $ FillError [] txt
                    Right stuff -> lift $ Right stuff

    modify (\x -> x { msgSize = 0} )

    filledCryptTail <- fillMsg cryptItems $ cryptMsgDef { msgItems = [last $ msgItems cryptMsgDef] }

    modify (\x -> x { msgSeq = 1 })
    filledCryptHead <- fillMsg cryptItems $ cryptMsgDef { msgItems = init $ msgItems cryptMsgDef }

    return (filledCryptHead ++ filledCryptTail)

decrypt :: MSGValue -> HbciReader MSGValue
decrypt msgVal = do
  cryptedResDef <- askMsgDef "CryptedRes"
  (_, known) <- return $! extractMsg cryptedResDef msgVal
  case lookup "CryptData.data" known of
    (Just (DEBinary bs)) -> fromEither $! parser bs >>= \x -> return (take 1 msgVal ++ x ++ drop (length msgVal -1) msgVal)
    _                    -> left $! HbciErrorInputData "Coundn't decrypt message"

-- FIXME: Need to uniquify the tan modes
findTanModes :: [(T.Text, DEValue)] -> [TanMode]
findTanModes response = catMaybes $ map mkTanMode modes
  where
    f i               = let nm = "TAN2StepPar" <> T.pack (show i) in (nm, filter (\x -> nm `T.isPrefixOf` (fst x)) response)
    modes             = filter (not . null . snd) $ map f  [1..(5::Int)]
    mkTanMode (nm,xs) = MkTanMode <$> (deToTxt =<< lookup (nm <> ".ParTAN2Step.secfunc") xs)
                                  <*> (deToTxt =<< lookup (nm <> ".ParTAN2Step.name") xs)
                                  <*> (deToTxt =<< lookup (nm <> ".ParTAN2Step.id") xs)

createMsg :: FormatTime t => t -> T.Text -> MSGEntry -> HbciReader BS.ByteString
createMsg time msgName msgVals = do
  msgDef <- askMsgDef msgName
  msgVals' <- if (msgRequiresSignature msgDef) then sign time msgVals else return msgVals
  if (msgRequiresEncryption msgDef)
    then gen <$> crypt time msgDef msgVals'
    else fromEither $! gen <$> (finalizeMsg $! fillMsg msgVals' msgDef)

sendMsg :: BS.ByteString -> HbciIO BS.ByteString
sendMsg msg = do
  HbciInfo props _ _ _ blz <- ask
  url <- fromMaybe (HbciErrorInputData ("Invalid BLZ: " <> blz)) (bankPinTanUrl <$> M.lookup blz props)
  liftIO $ C8.putStrLn $ "Sending:  " <> msg
  response <- liftIO $! NW.sendMsg url msg
  liftIO $ C8.putStrLn $ "Received: " <> response
  return response

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

processResponse :: T.Text -> BS.ByteString -> Hbci [(T.Text, DEValue)]
processResponse msgName msg = do
  responseDef <- liftReader $! askMsgDef (msgName <> "Res")

  parsedMsg <- fromEither $! parser msg
  decryptedMsg <- liftReader $! if msgRequiresEncryption responseDef then decrypt parsedMsg else return parsedMsg
  (unknownRes, knownRes) <- return $! extractMsg responseDef decryptedMsg

  when (not $! null unknownRes) $! left $! HbciErrorInternal $! msgName <> "Res: Unknown response"

  -- FIXME: This is crap ... these must be able to fail and report an error ...
  HbciState bpd upd _ _ sysId tanModes <- get
  let bpd'      = updateBPD knownRes <|> bpd
      upd'      = updateUPD knownRes <|> upd
      sysId'    = (deToTxt =<< lookup "SyncRes.sysid" knownRes) <|> sysId
      tanModes' = let x = findTanModes knownRes in if null x then tanModes else x

  dialogId' <- fromMaybe (HbciErrorInternal "SynchResponse: new dialogid could not be found") $ deToTxt =<< lookup "MsgHead.dialogid" knownRes

  -- FIXME: I probably only want to do this for some messages...
  modify (\st -> st { hbciStateBPD = bpd'
                    , hbciStateUPD = upd'
                    , hbciStateDialogID = dialogId'
                    , hbciStateMsgNum = hbciStateMsgNum st + 1
                    , hbciStateSysId = sysId'
                    , hbciStateTanModes = tanModes'
                    })

  return knownRes

sendSync :: HbciIO ()
sendSync  = do
  -- Synch.Idn.KIK.blz to "10050000"
  -- Synch.Idn.KIK.country to "DE"
  -- Synch.Idn.customerid to "6015813332M"
  -- Synch.Idn.sysid to "0"
  -- Synch.Idn.sysStatus to "1"
  -- Synch.MsgHead.dialogid to "0"
  -- Synch.MsgHead.msgnum to "1"
  -- Synch.MsgTail.msgnum to "1"
  -- Synch.ProcPrep.BPD to "49"
  -- Synch.ProcPrep.UPD to "0"
  -- Synch.ProcPrep.lang to "0"
  -- Synch.ProcPrep.prodName to "HBCI4Java"
  -- Synch.ProcPrep.prodVersion to "2.5"
  -- Synch.Sync.mode to "0"
  HbciInfo _ _ userId _ blz <- ask
  HbciState bpd upd _ _ _ _ <- get

  let msgVals = M.fromList
                [("Idn", M.fromList [("KIK", DEGentry $ M.fromList [("country", DEStr "280"), ("blz", DEStr blz)])
                                    ,("customerid", DEentry $ DEStr userId)
                                    ,("sysid", DEentry $ DEStr "0")
                                    ,("sysStatus", DEentry $ DEStr "1")])
                ,("ProcPrep", M.fromList [("BPD", DEentry $ DEStr $ maybe "0" bpdVersion bpd)
                                         ,("UPD", DEentry $ DEStr $ maybe "0" updVersion upd)
                                         ,("lang", DEentry $ DEStr "1")
                                         ,("prodName", DEentry $ DEStr "HsBCI")
                                         ,("prodVersion", DEentry $ DEStr "0.1")])
                ,("Sync", M.fromList [("mode", DEentry $ DEStr "0")])
                ]

  ZonedTime time _ <- liftIO $ getZonedTime
  response <- sendMsg =<< (liftReader $! createMsg time "Synch" msgVals)
  _ <- liftHbci $! processResponse "Synch" response
  return ()


sendDialogInit :: HbciIO ()
sendDialogInit = do
  HbciInfo _ _ userId _ blz <- ask
  HbciState bpd upd _ _ sysId _ <- get
  modify (\st -> st { hbciStateDialogID = "0", hbciStateMsgNum = 1 })

  let msgVals = M.fromList
                [("Idn", M.fromList [("KIK", DEGentry $ M.fromList [("country", DEStr "280"), ("blz", DEStr blz)])
                                    ,("customerid", DEentry $ DEStr userId)
                                    ,("sysid", DEentry $ DEStr $ maybe "0" id sysId)
                                    ,("sysStatus", DEentry $ DEStr "1")])
                ,("ProcPrep", M.fromList [("BPD", DEentry $ DEStr $ maybe "0" bpdVersion bpd)
                                         ,("UPD", DEentry $ DEStr $ maybe "0" updVersion upd)
                                         ,("lang", DEentry $ DEStr "1")
                                         ,("prodName", DEentry $ DEStr "HsBCI")
                                         ,("prodVersion", DEentry $ DEStr "0.1")])
                ]

  ZonedTime time _ <- liftIO $ getZonedTime
  response <- sendMsg =<< (liftReader $! createMsg time "DialogInit" msgVals)
  _ <- liftHbci $! processResponse "DialogInit" response
  return ()

sendDialogEnd :: HbciIO ()
sendDialogEnd = do
  HbciState _ _ dialogId msgnum _ _ <- get

  let msgVals = M.fromList [("MsgHead", M.fromList [("dialogid", DEentry $ DEStr dialogId), ("msgnum", DEentry $ DEStr $ T.pack $ show msgnum)])
                           ,("DialogEndS", M.fromList [("dialogid", DEentry $ DEStr dialogId)])
                           ,("MsgTail", M.fromList [("dialogid", DEentry $ DEStr dialogId), ("msgnum", DEentry $ DEStr $ T.pack $ show msgnum)])
                           ]

  ZonedTime time _ <- liftIO $ getZonedTime
  response <- sendMsg =<< (liftReader $! createMsg time "DialogEnd" msgVals)
  _ <- liftHbci $! processResponse "DialogEnd" response
  return ()


-- FIXME: the name is crap
sendJobsInternal :: Job x => x -> HbciIO (JobResult x)
sendJobsInternal jobs = do
  HbciState _ _ dialogId msgnum _ _ <- get
  params <- liftReader $! getParams jobs
  let msgVals = M.union params (M.fromList [("MsgHead", M.fromList [("dialogid", DEentry $ DEStr dialogId)
                                                                   ,("msgnum", DEentry $ DEStr $ T.pack $ show msgnum)])
                                           ,("MsgTail", M.fromList [("dialogid", DEentry $ DEStr dialogId)
                                                                   ,("msgnum", DEentry $ DEStr $ T.pack $ show msgnum)])])
  ZonedTime time _ <- liftIO $ getZonedTime
  response <- sendMsg =<< (liftReader $! createMsg time "CustomMsg" msgVals)
  response' <- liftHbci $! processResponse "CustomMsg" response
  liftHbci $! getResult jobs response'


getHbciConfig :: IO (Either String (M.Map T.Text BankProperties, M.Map T.Text MSG))
getHbciConfig = do
  bankProps <- getBankPropsFromFile "resources/blz.properties"
  -- FIXME: Read in all supported hbci versions
  msgs      <- getMSGfromXML <$> getXml ("resources/hbci-plus.xml")
  case (bankProps, msgs) of
    (Right bankProps', Right msgs') -> return $ Right $ (bankProps', msgs')
    _                               -> return $ Left "HsBCI: Error getting config"

-- Example 1 - simple command line client
main :: IO ()
main = do
  putStrLn "Please enter your BLZ, UserID, account number, pin (separated by ','):"
  [blz, uid, accnum, pin] <- T.splitOn "," <$> T.pack <$> getLine

  (props, msgDefs) <- do
    maybeConf <- getHbciConfig
    case maybeConf of
      (Left e) -> putStrLn (show e) >> exitFailure
      (Right cfg) -> return cfg

  let hbciInfo  = HbciInfo props msgDefs uid pin blz

  hbciRes <- evalHbciIO hbciInfo initialHbciState $ sendHbciJobs (GetBalance accnum)

  case hbciRes of
    Left e                           -> putStrLn ("ERROR: " ++ show e) >> exitFailure
    Right (GetBalanceResult bal _ _) -> putStrLn $ "Found balance: " ++ show bal
