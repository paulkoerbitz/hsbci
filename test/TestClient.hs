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

import           Data.HBCI.HbciDef
import           Data.HBCI.Messages
import           Data.HBCI.Gen
import           Data.HBCI.Parser

addr :: String
-- addr = "https://hbci.postbank.de/banking/hbci.do"
addr = "https://hbci-pintan-by.s-hbci.de/PinTanServlet"

-- 76010085=Postbank|Nürnberg, Mittelfr|PBNKDEFF760|24||https://hbci.postbank.de/banking/hbci.do|220|plus|
-- 38250110=Kreissparkasse Euskirchen|Euskirchen|WELADED1EUS|00|i031.rl.s-hbci.de|https://hbci-pintan-rl.s-hbci.de/PinTanServlet|220|plus|


msgVals :: M.Map T.Text T.Text
msgVals = M.fromList [("Idn.country", "280")
                     -- ,("Idn.blz", "76010085")
                     ,("Idn.blz", "12030000")
                     ,("BPD", "0")
                     ,("UPD", "0")
                     ,("lang", "0")
                     ,("prodName", "HsBCI")
                     ,("prodVersion", "0.1")]

main :: IO ()
main = do
  hbciPlus <- getXml "resources/hbci-plus.xml" >>= return . getMSGfromXML
  case hbciPlus of
    Left err -> TIO.putStrLn err
    Right defs -> do
      dialogInitAnonDef <- maybe (putStrLn "Error: Can't find 'DialogInitAnon'" >> exitFailure) return (M.lookup "DialogInitAnon" defs)
      let msg' = gen <$> fillMsg msgVals dialogInitAnonDef
      case msg' of
        Left err -> TIO.putStrLn ("Error: " <> err) >> exitFailure
        Right msg -> do
          request' <- parseUrl addr
          let request = request' { method = "POST"
                                 , requestHeaders = ("Content-Type", "application/octet-stream"): requestHeaders request'
                                 , requestBody = RequestBodyBS msg
                                 }
          response <- withManager $ httpLbs request
          putStrLn (show $ responseBody response) >> exitSuccess
