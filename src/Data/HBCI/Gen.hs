{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Data.HBCI.Gen where

import           Data.Monoid ((<>))

-- import qualified Data.Vector as V
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.HBCI.Types

gen :: SF -> BS.ByteString
gen segs = let res = BS.intercalate "'" (map genSEG segs)
           in if (BS.null res) then res else res <> "'"

genSEG :: SEG -> BS.ByteString
genSEG seg = BS.intercalate "+" $ map genDEG seg

genDEG :: DEG -> BS.ByteString
genDEG deg = BS.intercalate ":" $ map genDE deg

genDE :: DE -> BS.ByteString
genDE (DEStr t)     = E.encodeUtf8 t -- FIXME: This should really be latin1
genDE (DEBinary bs) = C8.singleton '@' <> (C8.pack $ show $ BS.length bs) <> C8.singleton '@' <> bs
