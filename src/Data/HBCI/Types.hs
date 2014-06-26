{-# LANGUAGE BangPatterns #-}
module Data.HBCI.Types where

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS

data DE = DEStr !T.Text
        | DEBinary !BS.ByteString
        deriving (Show, Eq)

type DEG = [DE]
type SEG = [DEG]
type SF = [SEG]
--
-- newtype DEG = MkDEG [DE]
--
-- newtype SEG = MkSEG [DEG]
--
-- newtype SF = MkSF [SEG]
