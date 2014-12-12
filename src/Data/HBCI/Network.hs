{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.Network where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text as T

import           Network.HTTP.Conduit

import           Data.HBCI.Types

-- FIXME: Error handling, timeouts, ...
sendMsg :: BankProperties -> BS.ByteString -> HBCI BS.ByteString
sendMsg props msg = do
  request' <- parseUrl $ T.unpack $ bankPinTanUrl props
  let request = request' { method = "POST"
                         , requestHeaders = ("Content-Type", "application/octet-stream"): requestHeaders request'
                         , requestBody = RequestBodyBS $ B64.encode msg
                         }
  response <- withManager $ httpLbs request
  return $! B64.decodeLenient $! BS.concat $! LBS.toChunks $! responseBody response
