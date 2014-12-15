{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (when, foldM)

import           Control.Monad.Trans        (lift, liftIO)
import           Control.Monad.Trans.Either (EitherT, runEitherT, left, right, hoistEither)
import           Control.Monad.State        (StateT, evalStateT)
import           Control.Monad.Reader       (ReaderT, runReaderT, ask)
import           Control.Monad.State        (get, put, modify)

import qualified Data.ByteString.Char8 as C8

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


import           Data.HBCI.Network
import           Data.HBCI.Messages
import           Data.HBCI.Gen
import           Data.HBCI.Parser
import           Data.HBCI.Constants

class Job x where
  type JobResult x :: *

  getParams :: HbciUserInfo -> x -> MSGEntry

  getResult :: x -> [(T.Text, DEValue)] -> Either HbciError (JobResult x)


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
  getParams (MkHbciUserInfo _ _ blz) (GetBalance accNum) =
    -- FIXME: Probably need to get the version of the supported saldo method from the BPD
    M.fromList [("Saldo5", M.fromList [("KTV", DEGentry $ M.fromList [("number", DEStr accNum), ("KIK.country", DEStr "280"), ("KIK.blz", DEStr blz)])
                                      ,("allaccounts", DEentry (DEStr "N"))
                                      ])
               ]

  getResult _job vals = do
    bal <- maybe (Left $ HbciErrorInternal "Saldo5: Didn't find expected field 'booked.BTG.value'") Right bookedBal
    return $! GetBalanceResult bal currentBal overdraft
    where
      -- FIXME: Could also lookup currency, account number to check
      -- it's actually for the correct account, etc...
      bookedBal  = btgToInt =<< lookup "SaldoRes5.booked.BTG.value" vals
      currentBal = btgToInt =<< lookup "SaldoRes5.pending.BTG.value" vals -- FIXME: Is this correct?
      overdraft  = btgToInt =<< lookup "SaldoRes5.kredit.value" vals


sendHbciJobs :: Job x => HbciUserInfo -> x -> HBCI (JobResult x)
sendHbciJobs ui jobs = do
  -- FIXME: what do I need to do here?
  -- * The dialog-init dialog-end handshake
  -- * If there are no BPD / UPD I need to fetch these
  -- * Generate the message which includes the respective jobs
  -- * Check the message length and the max number of jobs (BPD),
  --   if it becomes too long chop it into several messages and collect those
  -- * Sign and crypt it if necessary
  -- * Extract the results from the responses and put them into the
  --   result collections.
  -- * So this is the full workhorse ;)
  sendSync ui

  sendDialogInit ui

  res <- sendJobsInternal ui jobs

  sendDialogEnd ui

  return res


fromMaybe :: HbciError -> Maybe a -> HBCI a
fromMaybe err m = lift $ lift $ maybe (left err) right m

fromEither :: Either T.Text a -> HBCI a
fromEither = lift . lift . hoistEither . first HbciErrorOther

first :: (a -> b) -> Either a c -> Either b c
first l (Left  x)  = Left (l x)
first _ (Right x)  = Right x

throw :: HbciError -> HBCI a
throw = lift . lift . left

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
updateBPD stuff = MkBPD <$> (deToTxt =<< lookup "BPA.version" stuff)
                        <*> (deToInt =<< lookup "BPA.numgva" stuff)
                        <*> (deToInt =<< lookup "BPA.maxmsgsize" stuff)
                        <*> Just (M.fromList stuff)

-- FIXME: Key is not right
updateUPD :: [(T.Text, DEValue)] -> Maybe UPD
updateUPD stuff = MkUPD <$> (deToTxt =<< lookup "UPA.version" stuff)

sign :: FormatTime t => t -> Maybe T.Text -> [TanMode] -> HbciUserInfo -> MSGEntry -> Either T.Text MSGEntry
sign localTime sysid tanModes (MkHbciUserInfo userID pin blz) msg =
  foldM (\acc (k,v) -> nestedInsert k (DEStr v) acc) msg
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

  ,(["SigHead", "KeyName", "userid"], userID)
  ,(["SigHead", "KeyName", "country"], "280")
  ,(["SigHead", "KeyName", "blz"], blz)
  ,(["SigHead", "KeyName", "keynum"], "0") -- It's what hbci4java does for PinTan ...
  ,(["SigHead", "KeyName", "keyversion"], "0") -- It's what hbci4java does for PinTan ...

  ,(["SigTail", "UserSig", "pin"], pin)
  ,(["SigTail", "seccheckref"], "1234567890")
  ]


crypt ::  FormatTime t => t -> HbciState -> HbciUserInfo -> MSG -> MSG -> MSGEntry -> Either T.Text MSGValue
crypt localTime (MkHbciState _ _ dialogID msgNum sysID _) (MkHbciUserInfo userID _pin blz) cryptMsgDef (MSG reqSig reqEnc items) entries = finalizeMsg $ do

  -- FIXME: This is carp ...
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
                      ,(["CryptHead", "SecIdnDetails", "sysid"], DEStr $ maybe "0" id sysID)
                       -- ,(["CryptHead", "SecIdnDetails", "cid"], DEStr "") for DDV

                      ,(["CryptHead", "SecTimestamp", "date"], DEStr $ T.pack $ formatTime defaultTimeLocale "%Y%m%d" localTime)
                      ,(["CryptHead", "SecTimestamp", "time"], DEStr $ T.pack $ formatTime defaultTimeLocale "%H%M%S" localTime)

                      ,(["CryptHead", "CryptAlg", "mode"], DEStr "2") -- FIXME
                      ,(["CryptHead", "CryptAlg", "alg"], DEStr "13") -- FIXME
                      ,(["CryptHead", "CryptAlg", "enckey"], DEBinary "\0\0\0\0\0\0\0\0")
                      ,(["CryptHead", "CryptAlg", "keytype"], DEStr "5") -- FIXME

                      ,(["CryptHead", "KeyName", "country"], DEStr "280")
                      ,(["CryptHead", "KeyName", "blz"], DEStr blz)
                      ,(["CryptHead", "KeyName", "userid"], DEStr userID)
                      ,(["CryptHead", "KeyName", "keynum"] ,DEStr "0") -- FIXME
                      ,(["CryptHead", "KeyName", "keyversion"], DEStr "0") -- FIXME

                      ,(["CryptHead", "compfunc"], DEStr "0") -- FIXME

                      ,(["CryptHead", "SecProfile", "method"], DEStr "1") -- FIXME
                      ,(["CryptHead", "SecProfile", "version"], DEStr "1") -- FIXME

                      ,(["CryptData","data"], DEBinary msgtext)
                      -- FIXME: why do we need this? I guess it should be automated when filling
                      ,(["MsgHead", "dialogid"], DEStr dialogID)
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


-- FIXME: These things should not be in HBCI, but they should have access to the config env
decrypt :: MSG -> MSGValue -> Either T.Text MSGValue
decrypt cryptedResDef msgVal =
  let (_, known) = extractMsg cryptedResDef msgVal
  in case lookup "CryptData.data" known of
    (Just (DEBinary bs)) -> parser bs >>= \x -> return (take 1 msgVal ++ x ++ drop (length msgVal -1) msgVal)
    _                    -> Left "Coundn't decrypt message"


-- FIXME: Need to uniquify the tan modes in some way ...
findTanModes :: [(T.Text, DEValue)] -> [TanMode]
findTanModes response = catMaybes $ map mkTanMode modes
  where
    f i               = let nm = "TAN2StepPar" <> T.pack (show i) in (nm, filter (\x -> nm `T.isPrefixOf` (fst x)) response)
    modes             = filter (not . null . snd) $ map f  [1..(5::Int)]
    mkTanMode (nm,xs) = MkTanMode <$> (deToTxt =<< lookup (nm <> ".ParTAN2Step.secfunc") xs)
                                  <*> (deToTxt =<< lookup (nm <> ".ParTAN2Step.name") xs)
                                  <*> (deToTxt =<< lookup (nm <> ".ParTAN2Step.id") xs)


sendSync :: HbciUserInfo -> HBCI ()
sendSync ui@(MkHbciUserInfo userID _ blz) = do
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

  MkHbciConfig bankProps hbciDef <- ask
  hbciSt@(MkHbciState bpd upd _did _msgnum sysid tanModes) <- get
  props <- fromMaybe (HbciErrorInputData ("Unknown BLZ: " <> blz)) $ M.lookup blz bankProps
  msgDef <- fromMaybe (HbciErrorInternal "Can't find definition of Synch") $ M.lookup "Synch" hbciDef
  cryptedMsgDef <- fromMaybe (HbciErrorInternal "Can't find definition of Crypted") $ M.lookup "Crypted" hbciDef
  cryptedResDef <- fromMaybe (HbciErrorInternal "Can't find definition of CryptedRes") $ M.lookup "CryptedRes" hbciDef
  ZonedTime localTime _ <- liftIO $ getZonedTime

  let msgVals = M.fromList
                [("Idn", M.fromList [("KIK", DEGentry $ M.fromList [("country", DEStr "280"), ("blz", DEStr blz)])
                                    ,("customerid", DEentry $ DEStr userID)
                                    ,("sysid", DEentry $ DEStr "0") -- $ maybe "0" id sysid)
                                    ,("sysStatus", DEentry $ DEStr "1")])
                ,("ProcPrep", M.fromList [("BPD", DEentry $ DEStr $ maybe "0" bpdVersion bpd)
                                         ,("UPD", DEentry $ DEStr $ maybe "0" updVersion upd)
                                         ,("lang", DEentry $ DEStr "1")
                                         ,("prodName", DEentry $ DEStr "HsBCI")
                                         ,("prodVersion", DEentry $ DEStr "0.1")])
                ,("Sync", M.fromList [("mode", DEentry $ DEStr "0")])
                ]

  -- msgDef <- fromMaybe (HbciErrorInternal "Can't find definition of DialogInitAnon") $ M.lookup "DialogInitAnon" hbciDef

  -- msg <-  fromEither $ gen <$> finalizeMsg (fillMsg msgVals msgDef)
  msg <- fromEither $ gen <$> (crypt localTime hbciSt ui cryptedMsgDef msgDef =<< sign localTime sysid tanModes ui msgVals)

  liftIO $ C8.putStrLn $ "Sending:  " <> msg
  -- FIXME: The only point where we need IO. This should be pulled out and the main monad should not be in IO
  cryptedSynchResponse <- sendMsg props msg
  liftIO $ C8.putStrLn $ "Received: " <> cryptedSynchResponse

  -- initResDef <- fromMaybe (HbciErrorInternal "Can't find definition of DialogInitAnonRes") $ M.lookup "DialogInitAnonRes" hbciDef
  synchResDef <- fromMaybe (HbciErrorInternal "Can't find definition of SynchRes") $ M.lookup "SynchRes" hbciDef

  (unknownRes, knownRes) <- fromEither $ return . extractMsg synchResDef =<< decrypt cryptedResDef =<< parser cryptedSynchResponse

  -- liftIO $ putStrLn $ show knownRes

  when (not $ null unknownRes) $ throw $ HbciErrorInternal "DialogInitAnonRes: Unknown response"

  -- FIXME: This is crap ... these must be able to fail and report an error ...
  let bpd'      = updateBPD knownRes
      upd'      = updateUPD knownRes
      sysID'    = deToTxt =<< lookup "SyncRes.sysid" knownRes
      tanModes' = findTanModes knownRes

  liftIO $ putStrLn $ "TanModes: " ++ show tanModes'

  -- Extract security function stuff
  -- tanModes <-

  dialogID' <- fromMaybe (HbciErrorInternal "SynchResponse: new dialogid could not be found") $ deToTxt =<< lookup "MsgHead.dialogid" knownRes
  -- sysID' <- fromMaybe (HbciErrorInternal "SynchResponse: new sysid could not be found") $ deToTxt =<< lookup "" knownRes


  -- modify (\x -> x { hbciStateBPD = bpd', hbciStateUPD = upd', hbciStateDialogID = dialogID', hbciStateMsgNum = hbciStateMsgNum x + 1, hbci
  put (MkHbciState bpd' upd' dialogID' (hbciStateMsgNum hbciSt + 1) sysID' tanModes')


sendDialogInit :: HbciUserInfo -> HBCI ()
sendDialogInit ui@(MkHbciUserInfo userID _ blz) = do
  -- FIXME: Create anon or non-anon dialog (when is anon supported?)

  -- DialogInit.Idn.KIK.blz to "10050000"
  -- DialogInit.Idn.KIK.country to "DE"
  -- DialogInit.Idn.customerid to "6015813332M"
  -- DialogInit.Idn.sysid to "5194392401811301"
  -- DialogInit.Idn.sysStatus to "1"
  -- DialogInit.ProcPrep.BPD to "49"
  -- DialogInit.ProcPrep.UPD to "0"
  -- DialogInit.ProcPrep.lang to "1"
  -- DialogInit.ProcPrep.prodName to "HBCI4Java"
  -- DialogInit.ProcPrep.prodVersion to "2.5"

  MkHbciConfig bankProps hbciDef <- ask
  MkHbciState bpd upd _did _msgnum sysid tanModes <- get
  props <- fromMaybe (HbciErrorInputData ("Unknown BLZ: " <> blz)) $ M.lookup blz bankProps
  msgDef <- fromMaybe (HbciErrorInternal "Can't find definition of DialogInit") $ M.lookup "DialogInit" hbciDef
  cryptedMsgDef <- fromMaybe (HbciErrorInternal "Can't find definition of Crypted") $ M.lookup "Crypted" hbciDef
  cryptedResDef <- fromMaybe (HbciErrorInternal "Can't find definition of CryptedRes") $ M.lookup "CryptedRes" hbciDef
  ZonedTime localTime _ <- liftIO $ getZonedTime

  let msgVals = M.fromList
                [("Idn", M.fromList [("KIK", DEGentry $ M.fromList [("country", DEStr "280"), ("blz", DEStr blz)])
                                    ,("customerid", DEentry $ DEStr userID)
                                    ,("sysid", DEentry $ DEStr $ maybe "0" id sysid)
                                    ,("sysStatus", DEentry $ DEStr "1")])
                ,("ProcPrep", M.fromList [("BPD", DEentry $ DEStr $ maybe "0" bpdVersion bpd)
                                         ,("UPD", DEentry $ DEStr $ maybe "0" updVersion upd)
                                         ,("lang", DEentry $ DEStr "1")
                                         ,("prodName", DEentry $ DEStr "HsBCI")
                                         ,("prodVersion", DEentry $ DEStr "0.1")])
                ]

  -- msgDef <- fromMaybe (HbciErrorInternal "Can't find definition of DialogInitAnon") $ M.lookup "DialogInitAnon" hbciDef

  -- msg <-  fromEither $ gen <$> finalizeMsg (fillMsg msgVals msgDef)
  msg <- fromEither $ gen <$> (crypt localTime (MkHbciState bpd upd "0" 1 sysid tanModes) ui cryptedMsgDef msgDef =<< sign localTime sysid tanModes ui msgVals)

  liftIO $ C8.putStrLn $ "Sending:  " <> msg
  initResponse <- sendMsg props msg
  liftIO $ C8.putStrLn $ "Received: " <> initResponse

  -- initResDef <- fromMaybe (HbciErrorInternal "Can't find definition of DialogInitAnonRes") $ M.lookup "DialogInitAnonRes" hbciDef
  initResDef <- fromMaybe (HbciErrorInternal "Can't find definition of DialogInitRes") $ M.lookup "DialogInitRes" hbciDef

  (unknownRes, knownRes) <- fromEither $ return . extractMsg initResDef =<< decrypt cryptedResDef =<< parser initResponse

  liftIO $ putStrLn $ show knownRes

  when (not $ null unknownRes) $ throw $ HbciErrorInternal "DialogInitAnonRes: Unknown response"

  -- FIXME: This is crap ... these must be able to fail and report an error ...
  let bpd'      = updateBPD knownRes
      upd'      = updateUPD knownRes

  dialogId' <- fromMaybe (HbciErrorInternal "Can't find the freaking dialogid") $ deToTxt =<< lookup "MsgHead.dialogid" knownRes

  put (MkHbciState bpd' upd' dialogId' 2 sysid tanModes)


sendDialogEnd :: HbciUserInfo -> HBCI ()
sendDialogEnd (MkHbciUserInfo _ _ blz) = do
  -- This works ;) (Needs some fixes of course)
  MkHbciConfig bankProps hbciDef <- ask
  MkHbciState _bpd _upd did msgnum _sysid _tanModes <- get

  -- FIXME: msgnum
  let msgVals = M.fromList [("MsgHead", M.fromList [("dialogid", DEentry $ DEStr did), ("msgnum", DEentry $ DEStr $ T.pack $ show msgnum)])
                           ,("DialogEndS", M.fromList [("dialogid", DEentry $ DEStr did)])
                           ,("MsgTail", M.fromList [("dialogid", DEentry $ DEStr did), ("msgnum", DEentry $ DEStr $ T.pack $ show msgnum)])
                           ]

  props <- fromMaybe (HbciErrorInputData ("Unknown BLZ: " <> blz)) $ M.lookup blz bankProps

  msgDef <- fromMaybe (HbciErrorInternal "Can't find definition of DialogEndAnon") $ M.lookup "DialogEndAnon" hbciDef

  msg <- fromEither $ gen <$> finalizeMsg (fillMsg msgVals msgDef)

  liftIO $ C8.putStrLn $ "Sending:  " <> msg
  endResponse <- sendMsg props msg
  liftIO $ C8.putStrLn $ "Received: " <> endResponse

  endResDef <- fromMaybe (HbciErrorInternal "Can't find definition of DialogEndAnonRes") $ M.lookup "DialogEndAnonRes" hbciDef

  (unknownRes, _knownRes) <- fromEither $ return . extractMsg endResDef =<< parser endResponse

  when (not $ null unknownRes) $ throw $ HbciErrorInternal "DialogEndAnonRes: Unknown response"


-- FIXME: the name is crap
sendJobsInternal :: Job x => HbciUserInfo -> x -> HBCI (JobResult x)
sendJobsInternal ui@(MkHbciUserInfo _ _ blz) jobs = do
  -- What do I need to do here?
  -- I need some way to split the tuples
  -- Extract the required parameters for each job
  -- Send jobs, extract return values and assign them
  -- to the job results
  MkHbciConfig bankProps hbciDef <- ask
  MkHbciState bpd upd did msgnum sysid tanModes <- get
  props <- fromMaybe (HbciErrorInputData ("Unknown BLZ: " <> blz)) $ M.lookup blz bankProps
  msgDef <- fromMaybe (HbciErrorInternal "Can't find definition of CustomMsg") $ M.lookup "CustomMsg" hbciDef
  msgResDef <- fromMaybe (HbciErrorInternal "Can't find definition of CustomMsgRes") $ M.lookup "CustomMsgRes" hbciDef
  cryptedMsgDef <- fromMaybe (HbciErrorInternal "Can't find definition of Crypted") $ M.lookup "Crypted" hbciDef
  cryptedResDef <- fromMaybe (HbciErrorInternal "Can't find definition of CryptedRes") $ M.lookup "CryptedRes" hbciDef
  ZonedTime localTime _ <- liftIO $ getZonedTime

  -- liftIO $ putStrLn $ "dialogID is: " ++ show did ++ ", messageNum: " ++ show msgnum

  let msgVals = M.fromList [("MsgHead", M.fromList [("dialogid", DEentry $ DEStr did), ("msgnum", DEentry $ DEStr $ T.pack $ show msgnum)])
                           ,("MsgTail", M.fromList [("dialogid", DEentry $ DEStr did), ("msgnum", DEentry $ DEStr $ T.pack $ show msgnum)])
                           ]
                `M.union` getParams ui jobs

  msg <- fromEither $ gen <$> (crypt localTime (MkHbciState bpd upd did msgnum sysid tanModes) ui cryptedMsgDef msgDef =<< sign localTime sysid tanModes ui msgVals)

  liftIO $ C8.putStrLn $ "Sending:  " <> msg
  resp <- sendMsg props msg
  liftIO $ C8.putStrLn $ "Received: " <> resp

  (unknownRes, knownRes) <- fromEither $ return . extractMsg msgResDef =<< decrypt cryptedResDef =<< parser resp

  liftIO $ putStrLn $ show knownRes

  when (not $ null unknownRes) $ throw $ HbciErrorInternal "ResponseDef: Unknown response"

  modify (\x -> x { hbciStateMsgNum = hbciStateMsgNum x + 1 })

  lift . lift . hoistEither $! getResult jobs knownRes


getHbciConfig :: IO (Either T.Text HbciConfig)
getHbciConfig = do
  -- FIXME: merge the two monads (?)
  bankProps <- getBankPropsFromFile "resources/blz.properties"
  -- FIXME: Read in all supported hbci versions
  msgs      <- getMSGfromXML <$> getXml ("resources/hbci-plus.xml")
  case (bankProps, msgs) of
    (Right bankProps', Right msgs') -> return $ Right $ MkHbciConfig bankProps' msgs'
    _                               -> return $ Left "HsBCI: Error getting config"

evalHbci :: HbciConfig -> HbciState ->  HBCI a -> IO (Either HbciError a)
evalHbci cfg st action = runEitherT $ evalStateT (runReaderT action cfg) st

-- Example 1 - simple command line client
main :: IO ()
main = do
  putStrLn "Please enter your BLZ, UserID, account number, pin (separated by ','):"
  [blz, uid, accnum, pin] <- T.splitOn "," <$> T.pack <$> getLine

  hbciConfig <- do
    maybeConf <- getHbciConfig
    case maybeConf of
      (Left e) -> putStrLn (show e) >> exitFailure
      (Right cfg) -> return cfg

  let userInfo  = MkHbciUserInfo { uiUserId = uid
                                 , uiPIN    = pin
                                 , uiBLZ    = blz
                                 }

  hbciRes <- evalHbci hbciConfig initialHbciState $ sendHbciJobs userInfo (GetBalance accnum)

  case hbciRes of
    Left e                           -> putStrLn ("ERROR: " ++ show e) >> exitFailure
    Right (GetBalanceResult bal _ _) -> putStrLn $ "Found balance: " ++ show bal
