{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import           Control.Monad (foldM)
import           Control.Monad.State (get, put, modify)
import           Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Data.Time.Format (formatTime)
import           Data.Time.LocalTime (getZonedTime, ZonedTime(..))
import           System.Locale (defaultTimeLocale)

import           Network.HTTP.Conduit
import           System.Exit (exitSuccess, exitFailure)

import           Data.HBCI.Gen
import           Data.HBCI.HbciDef
import           Data.HBCI.Messages
import           Data.HBCI.Parser
import           Data.HBCI.Types

import Debug.Trace

msgVals :: MSGEntry
msgVals = M.fromList [("Idn", M.fromList [("KIK", DEGentry $ M.fromList [("country", DEStr "280")])])
                     ,("ProcPrep", M.fromList [("BPD", DEentry $ DEStr "41")
                                              ,("UPD", DEentry $ DEStr "0")
                                              ,("lang", DEentry $ DEStr "0")
                                              ,("prodName", DEentry $ DEStr "HsBCI")
                                              ,("prodVersion", DEentry $ DEStr "0.1.0")
                                              ])
                     ]

exitWMsg :: T.Text -> IO a
exitWMsg msg = TIO.putStrLn ("ERROR: " <> msg) >> exitFailure

sendMsg :: BankProperties -> BS.ByteString -> IO BS.ByteString
sendMsg props msg = do
  request' <- parseUrl $ T.unpack $ bankPinTanUrl props
  let request = request' { method = "POST"
                         , requestHeaders = ("Content-Type", "application/octet-stream"): requestHeaders request'
                         , requestBody = RequestBodyBS $ B64.encode msg
                         }
  response <- withManager $ httpLbs request
  return $! B64.decodeLenient $! BS.concat $! LBS.toChunks $! responseBody response

fromEither :: Either T.Text a -> IO a
fromEither = either exitWMsg return

bpdSegNames :: S.Set T.Text
bpdSegNames = S.fromList $
              [ "BPA"
              , "CommListRes"
              , "SecMethod"
              , "AccInfoPar1"
              , "AccInfoPar2"
              , "CardListPar1"
              , "CardListPar2"
              , "ChangePINPar1"
              , "ChangePINOldPar1"
              , "CommListPar2"
              , "CommListPar3"
              , "CommListPar4"
              , "CustomMsgPar2"
              , "CustomMsgPar3"
              , "CustomMsgPar4"
              , "CustomMsgPar5"
              , "DauerDelPar1"
              , "DauerDelPar2"
              , "DauerDelPar3"
              , "DauerDelPar4"
              , "DauerSEPADelPar1"
              , "DauerEditPar2"
              , "DauerEditPar3"
              , "DauerEditPar4"
              , "DauerEditPar5"
              , "DauerSEPAEditPar1"
              , "DauerListPar1"
              , "DauerListPar2"
              , "DauerListPar3"
              , "DauerListPar4"
              , "DauerListPar5"
              , "DauerSEPAListPar1"
              , "DauerNewPar2"
              , "DauerNewPar3"
              , "DauerNewPar4"
              , "DauerNewPar5"
              , "DauerSEPANewPar1"
              , "FestCondListPar1"
              , "FestCondListPar2"
              , "FestCondListPar3"
              , "FestListPar2"
              , "FestListPar3"
              , "FestListPar4"
              , "FestNewPar2"
              , "FestNewPar3"
              , "FestNewPar4"
              , "InfoDetailsPar1"
              , "InfoDetailsPar2"
              , "InfoDetailsPar3"
              , "InfoDetailsPar4"
              , "InfoListPar1"
              , "InfoListPar2"
              , "InfoListPar3"
              , "InfoListPar4"
              , "KontoauszugPar1"
              , "KontoauszugPar2"
              , "KontoauszugPar3"
              , "KontoauszugSEPAPar4"
              , "KUmsNewPar4"
              , "KUmsNewPar5"
              , "KUmsNewPar6"
              , "KUmsNewSEPAPar7"
              , "KUmsZeitPar4"
              , "KUmsZeitPar5"
              , "KUmsZeitPar6"
              , "KUmsZeitSEPAPar7"
              , "LastPar2"
              , "LastPar3"
              , "LastPar4"
              , "LastPar5"
              , "LastSEPAPar1"
              , "LastCOR1SEPAPar1"
              , "LastB2BSEPAPar1"
              , "LastObjectionPar1"
              , "LastObjectionPar2"
              , "OrderHistoryPar1"
              , "PinTanPar1"
              , "PinTanPar2"
              , "SaldoPar3"
              , "SaldoPar4"
              , "SaldoPar5"
              , "SaldoPar6"
              , "SaldoSEPAPar7"
              , "SammelLastPar3"
              , "SammelLastPar4"
              , "SammelLastPar5"
              , "SammelLastPar6"
              , "SammelUebPar3"
              , "SammelUebPar4"
              , "SammelUebPar5"
              , "SammelUebPar6"
              , "SammelUebEilPar1"
              , "SammelUebSEPAPar1"
              , "SammelLastSEPAPar1"
              , "SammelLastCOR1SEPAPar1"
              , "SammelLastB2BSEPAPar1"
              , "SEPAInfoPar1"
              , "StatusPar2"
              , "StatusPar3"
              , "StatusPar4"
              , "TAN2StepPar1"
              , "TAN2StepPar2"
              , "TAN2StepPar3"
              , "TAN2StepPar4"
              , "TAN2StepPar5"
              , "TANListListPar1"
              , "TANMediaListPar1"
              , "TANMediaListPar2"
              , "TANMediaListPar3"
              , "TANMediaListPar4"
              , "TermSammelLastPar3"
              , "TermSammelLastDelPar3"
              , "TermSammelLastListPar3"
              , "TermSammelUebPar3"
              , "TermSammelUebDelPar3"
              , "TermSammelUebListPar3"
              , "TermUebPar2"
              , "TermUebPar3"
              , "TermUebPar4"
              , "TermUebSEPAPar1"
              , "TermUebDelPar1"
              , "TermUebDelPar2"
              , "TermUebDelPar3"
              , "TermUebEditPar2"
              , "TermUebEditPar3"
              , "TermUebEditPar4"
              , "TermUebListPar1"
              , "TermUebListPar2"
              , "TermUebListPar3"
              , "TermUebSEPAListPar1"
              , "UebPar2"
              , "UebPar3"
              , "UebPar4"
              , "UebPar5"
              , "UebEilPar1"
              , "UebForeignPar1"
              , "UebForeignPar2"
              , "UebGarPar1"
              , "UebSEPAPar1"
              , "UmbPar1"
              , "UmbPar2"
              , "VormerkpostenPar1"
              , "WPDepotListPar2"
              , "WPDepotListPar3"
              , "WPDepotListPar4"
              , "WPDepotListPar5"
              , "WPDepotListPar6"
              , "WPDepotUmsPar1"
              , "WPDepotUmsPar2"
              , "WPDepotUmsPar3"
              , "WPDepotUmsPar4"
              , "WPDepotUmsPar5"
              , "WPInfoListPar1"
              , "WPInfoListPar2"
              , "WPInfoListPar3"
              , "WPKursListPar1"
              , "WPKursListPar2"
              , "WPKursListPar3"
              , "WPRefListPar1"
              , "WPRefListPar2"
              , "WPRefListPar3"
              , "WPStammListPar1"
              , "WPStammListPar2"
              , "WPStammListPar3"
              , "Template2DPar"
              , "TemplateDPar"
              , "Template2Par"
              , "TemplatePar"
              ]


------------------------------------------------------------------------
-- Signature related constants
------------------------------------------------------------------------
secfunc_hbci_sig_rdh :: T.Text
secfunc_hbci_sig_rdh = "1"

secfunc_hbci_sig_ddv :: T.Text
secfunc_hbci_sig_ddv = "2"

secfunc_fints_sig_dig :: T.Text
secfunc_fints_sig_dig = "1"

secfunc_fints_sig_sig :: T.Text
secfunc_fints_sig_sig = "2"


secfunc_sig_pt_1step :: T.Text
secfunc_sig_pt_1step = "999"

secfunc_sig_pt_2step_min :: T.Text
secfunc_sig_pt_2step_min = "900"

secfunc_sig_pt_2step_max :: T.Text
secfunc_sig_pt_2step_max = "997"


hashalg_sha1 :: T.Text
hashalg_sha1 = "1"

hashalg_sha256 :: T.Text
hashalg_sha256 = "3"

hashalg_sha384 :: T.Text
hashalg_sha384 = "4"

hashalg_sha512 :: T.Text
hashalg_sha512 = "5"

hashalg_sha256_sha256 :: T.Text
hashalg_sha256_sha256 = "6"

hashalg_ripemd160 :: T.Text
hashalg_ripemd160 = "999"


sigalg_des :: T.Text
sigalg_des = "1"

sigalg_rsa :: T.Text
sigalg_rsa = "10"


sigmode_iso9796_1 :: T.Text
sigmode_iso9796_1 = "16"

sigmode_iso9796_2 :: T.Text
sigmode_iso9796_2 = "17"

sigmode_pkcs1 :: T.Text
sigmode_pkcs1 = "18"

sigmode_pss :: T.Text
sigmode_pss = "19"

sigmode_retail_mac :: T.Text
sigmode_retail_mac = "999"
------------------------------------------------------------------------
-- End of Signature related constants
------------------------------------------------------------------------


main :: IO ()
main = do
  let blz    = "12030000"
  -- let userID = "17863762"
  -- let pin    = "12345"
  -- putStrLn "Please enter your BLZ:"
  -- blz <- T.pack <$> getLine
  --
  putStrLn "Please enter your User ID:"
  userID <- T.pack <$> getLine
  --
  putStrLn "Please enter your PIN:"
  pin <- T.pack <$> getLine

  bankProps <- getBankPropsFromFile "resources/blz.properties" >>= either (\err -> TIO.putStrLn err >> exitFailure) return
  props <- maybe (TIO.putStrLn ("Unknown BLZ: " <> blz) >> exitFailure) return (M.lookup blz bankProps)
  xml <- getXml ("resources/hbci-" <> (T.unpack $ bankPinTanVersion props) <> ".xml")
  hbciDef <- either exitWMsg return $ getMSGfromXML xml

  -- dialogInitAnonDef <- maybe (exitWMsg "Error: Can't find 'DialogInitAnon'") return $ M.lookup "DialogInitAnon" hbciDef
  -- dialogInitAnonVals <- fromEither $ foldM (\acc (k,v) -> nestedInsert k (DEStr v) acc) msgVals
  --                       [(["Idn","KIK","blz"], blz)
  --                       ,(["Idn","customerid"], userID)]
  -- dialogInitAnonMsg <- fromEither $ gen <$> fillMsg dialogInitAnonVals dialogInitAnonDef
  --
  -- C8.putStrLn $ "Message to be send:\n" <> dialogInitAnonMsg
  -- dialogInitAnonResponse <- sendMsg props dialogInitAnonMsg
  -- C8.putStrLn $ "Message received:\n" <> dialogInitAnonResponse
  --
  -- dialogInitAnonResDef <- maybe (exitWMsg "ERROR: Can't find 'DialogInitAnonRes'") return $ M.lookup "DialogInitAnonRes" hbciDef
  -- initAnonRes <- fromEither $ return . extractMsg dialogInitAnonResDef =<< parser dialogInitAnonResponse
  -- let bpd = filter (\(x,_) -> S.member (fst $ T.breakOn "." x) bpdSegNames) $ snd initAnonRes
  -- putStrLn $ "BPD: " ++ show bpd

  ZonedTime localTime _ <- getZonedTime

  let sign msg = fromEither $
                    foldM (\acc (k,v) -> nestedInsert k (DEStr v) acc) msg
                      [(["SigHead", "secfunc"], secfunc_sig_pt_1step) -- Must be '1' or '2', I'll try '1' here
                      ,(["SigHead", "seccheckref"], "12345678901234") -- Random number which must ocurr both in head and tail
                      ,(["SigHead", "range"], "1") -- This must be 1 apparently
                      ,(["SigHead", "role"], "1") -- 1, 3 or 4. 1: Issuer, 3: Cosigned, 4: Witness. Probably always 1.
                      ,(["SigHead", "SecIdnDetails", "func"], "1") -- 1 for User messages, 2 for 'XyzRes'
                       -- Something binary, can be empty, we'll leave this out for now.
                       -- Hbci4Java sets this for DDV and RDH passports, I think to a random number, can be left out for now
                       -- ,(["SigHead", "SecIdnDetails", "cid"], "0")
                      ,(["SigHead", "SecIdnDetails", "sysid"], "0") -- must be obtained with a sync call -- maybe hbci does this before

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
                      ,(["SigTail", "seccheckref"], "12345678901234")
                      ]

      -- PinTan encryption. It doesn't really encrypt anything, it just chops of the
      -- head and tail of a message, renders it and embeds it in another message ('crypted message')
      crypt :: MSG -> MSGEntry -> MSG -> Either T.Text MSGValue
      crypt (MSG reqSig reqEnc items) entries cryptMsgDef = finalizeMsg $ do

        let items'     = tail $ init $ items -- FIXME: using unsafe tail and init is really bad style ...
        put (MkFillState 0 2)
        msgtext <- gen <$> fillMsg entries (MSG reqSig reqEnc items')

        cryptItems <- case (foldM (\acc (k,v) -> nestedInsert k v acc) M.empty
                      [(["CryptHead", "secfunc"] , DEStr "998") -- FIXME
                      ,(["CryptHead", "role"], DEStr "1") -- FIXME

                      ,(["CryptHead", "SecIdnDetails", "func"], DEStr "1") -- "2" for Responses
                      ,(["CryptHead", "SecIdnDetails", "sysid"], DEStr "0")
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
                      ,(["MsgHead", "dialogid"], DEStr "0")
                      ,(["MsgHead", "msgnum"], DEStr "1")
                      ,(["MsgTail", "msgnum"], DEStr "1")
                      ]) of
                        Left txt -> lift $ Left $ FillError [] txt
                        Right stuff -> lift $ Right $ stuff

        get >>= \x -> trace (show x) (return ())
        modify (\x -> x { msgSize = 0} )
        get >>= \x -> trace (show x) (return ())

        filledCryptTail <- fillMsg cryptItems $ cryptMsgDef { msgItems = [last $ msgItems cryptMsgDef] }

        get >>= \x -> trace (show x) (return ())

        modify (\x -> x { msgSeq = 1 })
        get >>= \x -> trace (show x) (return ())
        filledCryptHead <- fillMsg cryptItems $ cryptMsgDef { msgItems = init $ msgItems cryptMsgDef }
        get >>= \x -> trace (show x) (return ())

        return (filledCryptHead ++ filledCryptTail)


  -- dialogInitDef <- maybe (exitWMsg "Error: Can't find 'DialogInit'") return $ M.lookup "DialogInit" hbciDef
  syncDef <- maybe (exitWMsg "Error: Can't find 'Synch'") return $ M.lookup "Synch" hbciDef
  cryptedMsgDef <- maybe (exitWMsg "Error: Can't find 'Crypted'") return $ M.lookup "Crypted" hbciDef

  dialogInitVals <- fromEither $ foldM (\acc (k,v) -> nestedInsert k (DEStr v) acc) msgVals
                    [(["Idn","KIK","blz"], blz)
                    ,(["Idn","customerid"], userID)
                    ,(["Idn","sysid"],      "0")
                     -- FIXME
                    ,(["Idn","sysStatus"], "1") -- 0 DDV, 1 for RDH

                    ,(["KeyReq", "KeyName", "userid"], userID)
                    ,(["KeyReq", "KeyName", "country"], "280")
                    ,(["KeyReq", "KeyName", "blz"], blz)
                    ,(["KeyReq", "KeyName", "keytype"], "S")
                    ,(["KeyReq", "KeyName", "keynum"], "0") -- It's what hbci4java does for PinTan ...
                    ,(["KeyReq", "KeyName", "keyversion"], "0") -- It's what hbci4java does for PinTan ...

                    ,(["Sync", "mode"], "0")
                    ]

  signedInitVals <- sign dialogInitVals
  cryptedSyncMsg <- fromEither $ gen <$> crypt syncDef signedInitVals cryptedMsgDef

  C8.putStrLn $ "Message to be send:\n" <> cryptedSyncMsg
  syncResponse <- sendMsg props cryptedSyncMsg
  C8.putStrLn $ "Message received:\n" <> syncResponse

  syncResDef <- maybe (exitWMsg "ERROR: Can't find 'SynchRes'") return $ M.lookup "SynchRes" hbciDef
  syncRes <- fromEither $ return . extractMsg syncResDef =<< parser syncResponse

  putStrLn $ show $ syncRes
  exitSuccess

-- Hbci4java:
-- HNHBK:1:3+000000000369+220+0+1'
-- HNVSK:998:2+998+1+1::0+1:20141031:171042+2:2:13:@8@^@^@^@^@^@^@^@^@:5:1+280:10050000:6015813332M:V:0:0+0'
-- HNVSD:999:1+@211@HNSHK:2:3+999+1922472290+1+1+1::0+1+1:20141031:171042+1:999:1+6:10:16+280:10050000:6015813332M:S:0:0'HKIDN:3:2+280:10050000+6015813332M+0+1'HKVVB:4:2+49+0+0+HBCI4Java+2.5'HKSYN:5:2+0'HNSHA:6:1+1922472290++XXXXX''
-- HNHBS:7:1+1'
