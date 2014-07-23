{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.Messages where

import           Control.Applicative ((<$>))
import           Data.Monoid ((<>))
import qualified Data.Map  as M
import           Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import           Data.Traversable (traverse)

import           Data.HBCI.Types

fillDe :: M.Map T.Text T.Text -> T.Text -> DE -> Either T.Text DEValue
fillDe _        _      (DEval v)                     = Right v
fillDe userVals prefix (DEdef deNm _ _ _ _ _ valids) =
  let key = if T.null prefix then deNm else prefix <> "." <> deNm
      mval = M.lookup key userVals
  in case mval of
    Nothing -> Left $ "Required key '" <> key <> "' missing in userVals"
    (Just val) -> if not (isJust valids) || val `elem` (fromJust valids)
                  then Right (DEStr val)
                  else Left ("Value '" <> val <> "' for key '" <> key <> "' not in valid values '" <> T.pack (show (fromJust valids)) <> "'")

fillSeg :: M.Map T.Text T.Text -> SEG -> Either T.Text SEGValue
fillSeg userVals (SEG segNm _ items) = traverse (fillSegItem userVals segNm) items

fillSegItem :: M.Map T.Text T.Text -> T.Text -> SEGItem -> Either T.Text DEGValue
fillSegItem userVals = go
  where
    go prefix (DEItem de)                        = (:[]) <$> fillDe userVals prefix de
    go prefix (DEGItem (DEG degnm _ _ degitems)) =
      let newPrefix = if T.null degnm then prefix else prefix <> "." <> degnm
      in traverse (fillDe userVals newPrefix) degitems

fillMsg :: M.Map T.Text T.Text -> MSG -> Either T.Text MSGValue
fillMsg userVals (MSG _reqSig _reqEnc items) = concat <$> traverse fillSf items
  where
    fillSf :: SF -> Either T.Text MSGValue
    fillSf (SF _ _ items) = traverse (fillSeg userVals) items
