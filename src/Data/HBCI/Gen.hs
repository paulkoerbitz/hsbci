{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Data.HBCI.Gen where

import           Data.Monoid ((<>))

import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.HBCI.Types

gen :: MSGValue -> BS.ByteString
gen segs = let res = BS.intercalate "'" (map genSEG segs)
           in if (BS.null res) then res else res <> "'"

genSEG :: SEGValue -> BS.ByteString
genSEG seg = BS.intercalate "+" $ map genDEG seg

genDEG :: DEGValue -> BS.ByteString
genDEG deg = BS.intercalate ":" $ map genDE deg

genDE :: DEValue -> BS.ByteString
genDE (DEStr t)     = E.encodeUtf8 t -- FIXME: This should really be latin1
genDE (DEBinary bs) = C8.singleton '@' <> (C8.pack $ show $ BS.length bs) <> C8.singleton '@' <> bs
