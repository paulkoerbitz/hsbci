{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Applicative ((<$>))
import           Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.HTTP.Conduit
import           System.Exit (exitSuccess, exitFailure)

import           Data.HBCI.HbciDef
import           Data.HBCI.Messages
import           Data.HBCI.Gen

dkbAddr :: String
dkbAddr = "https://hbci-pintan-by.s-hbci.de/PinTanServlet"

msgVals :: M.Map T.Text T.Text
msgVals = M.fromList [("Idn.country", "0"), ("BPD", "0"), ("UPD", "0"), ("lang", "0")
                     ,("prodName", "HsBCI"), ("prodVersion", "0.1.0")]

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
          request' <- parseUrl dkbAddr
          let request = request' { method = "POST", requestBody = RequestBodyBS msg }
          response <- withManager $ httpLbs request
          putStrLn (show response) >> exitSuccess
