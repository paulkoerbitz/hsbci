{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.Messages where

import           Control.Applicative ((<$>))
import           Data.Monoid ((<>))
import qualified Data.Map  as M
import           Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import           Data.Traversable (traverse)

import           Data.HBCI.Types

-- what do I need to do? Walk through the defs, remember which path I
-- am looking at and set the values found in the map. Set defaults where
-- values are not present and defaults are OK, return an error otherwise
fillDeg :: M.Map T.Text T.Text -> T.Text -> DEGItem -> Either T.Text DEGValue
fillDeg userVals = go
  where
    go prefix (DEG degNm _ _ (DEGdef _ items)) = concat <$> traverse (go (prefix <> "." <> degNm)) items
    go prefix (DE deNm _ _ _ _ _ valids) =
      let key = prefix <> "." <> deNm
          mval = M.lookup key userVals
      in case mval of
        Nothing -> Left ("Key '" <> key <> "' missing in userVals")
        (Just val) -> if not (isJust valids) || val `elem` (fromJust valids)
                      then (Right [DEStr val])
                      else Left ("Value '" <> val <> "' for key '" <> key <> "' not in valid values '" <> T.pack (show (fromJust valids)) <> "'")
    go _ (DEVal de) = Right [de]

fillMsg :: M.Map T.Text T.Text -> MSGdef -> Either T.Text MSGValue
fillMsg userVals (MSGdef reqSig reqEnc items) = traverse fillSeg items
  where
    fillSeg (SEG nm _ _ (SEGdef _ defs)) = traverse (fillDeg userVals nm) defs
