{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.HTTP.Conduit
import           System.Exit (exitSuccess, exitFailure)

import           Data.HBCI.Types
import           Data.HBCI.HbciDef
import           Data.HBCI.Messages
import           Data.HBCI.Gen
import           Data.HBCI.Parser

msgVals :: MSGEntry
msgVals = M.fromList [("Idn", M.fromList [("KIK", DEGentry $ M.fromList [("country", DEStr "0")])])
                     ,("ProcPrep", M.fromList [("BPD", DEentry $ DEStr "0")
                                              ,("UPD", DEentry $ DEStr "0")
                                              ,("lang", DEentry $ DEStr "0")
                                              ,("prodName", DEentry $ DEStr "HsBCI")
                                              ,("prodVersion", DEentry $ DEStr "0.1.0")
                                              ])
                     ]

main :: IO ()
main = do
  bankProps <- getBankPropsFromFile "resources/blz.properties" >>= either (\err -> TIO.putStrLn err >> exitFailure) return
  putStrLn "Please enter your BLZ:"
  blz <- T.pack <$> getLine
  props <- maybe (TIO.putStrLn ("Unknown BLZ: " <> blz) >> exitFailure) return (M.lookup blz bankProps)
  hbciDef <- getXml ("resources/hbci-" <> (T.unpack $ bankPinTanVersion props) <> ".xml") >>= return . getMSGfromXML
  case hbciDef of
    Left err -> TIO.putStrLn err
    Right defs -> do
      dialogInitAnonDef <- maybe (putStrLn "Error: Can't find 'DialogInitAnon'" >> exitFailure) return (M.lookup "DialogInitAnon" defs)
      let msg' = nestedInsert ["Idn","KIK","blz"] (DEStr blz) msgVals >>= (\x -> gen <$> fillMsg x dialogInitAnonDef)
      case msg' of
        Left err -> TIO.putStrLn ("ERROR: " <> err) >> exitFailure
        Right msg -> do
          BS.putStrLn $ "Message to be sent:\n" <> msg
          request' <- parseUrl $ T.unpack $ bankPinTanUrl props
          let request = request' { method = "POST"
                                 , requestHeaders = ("Content-Type", "application/octet-stream"): requestHeaders request'
                                 , requestBody = RequestBodyBS $ B64.encode msg
                                 }
          response <- withManager $ httpLbs request
          case (M.lookup "DialogInitAnonRes" defs, parser $ B64.decodeLenient $ BS.concat $ LBS.toChunks $ responseBody response) of
            (Nothing     , _ )             -> putStrLn "ERROR: Can't find 'DialogInitAnonRes' in definitions" >> exitFailure
            (Just _      , Left err)       -> TIO.putStrLn ("ERROR: Can't parse response: " <> err) >> exitFailure
            (Just diarDef, Right parseRes) -> (putStrLn $ show $ extractMsg diarDef parseRes) >> exitSuccess
