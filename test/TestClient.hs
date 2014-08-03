{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>))
import qualified Data.ByteString as BS
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

msgVals :: M.Map T.Text T.Text
msgVals = M.fromList [("Idn.country", "280")
                     ,("BPD", "0")
                     ,("UPD", "0")
                     ,("lang", "0")
                     ,("prodName", "HsBCI")
                     ,("prodVersion", "0.1")]

main :: IO ()
main = do
  bankProps <- getBankPropsFromFile "resources/blz.properties" >>= either (\err -> TIO.putStrLn err >> exitFailure) return
  putStrLn "Please enter your BLZ:"
  blz <- T.pack <$> getLine
  props <- maybe (TIO.putStrLn ("Unknown BLZ: " <> blz) >> exitFailure) return (M.lookup blz bankProps)
  hbciPlus <- getXml ("resources/hbci-" <> (T.unpack $ bankPinTanVersion props) <> ".xml") >>= return . getMSGfromXML
  case hbciPlus of
    Left err -> TIO.putStrLn err
    Right defs -> do
      dialogInitAnonDef <- maybe (putStrLn "Error: Can't find 'DialogInitAnon'" >> exitFailure) return (M.lookup "DialogInitAnon" defs)
      let msgVals' = M.insert "Idn.blz" blz msgVals
          msg' = gen <$> fillMsg msgVals' dialogInitAnonDef
      case msg' of
        Left err -> TIO.putStrLn ("Error: " <> err) >> exitFailure
        Right msg -> do
          request' <- parseUrl (T.unpack $ bankPinTanUrl props)
          let request = request' { method = "POST"
                                 , requestHeaders = ("Content-Type", "application/octet-stream"): requestHeaders request'
                                 , requestBody = RequestBodyBS msg
                                 }
          response <- withManager $ httpLbs request
          putStrLn (show $ responseBody response) >> exitSuccess