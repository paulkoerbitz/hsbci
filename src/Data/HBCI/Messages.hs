{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.Messages where

import           Control.Applicative ((<$>))
import           Data.Monoid ((<>))
import qualified Data.Map  as M
import           Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Traversable (traverse)

import           Data.HBCI.Types

isBinaryType :: DEType -> Bool
isBinaryType tp = tp == Bin || tp == DTAUS

escape :: T.Text -> T.Text
escape = T.foldl' (\txt c -> if c == '?' || c == '@' || c == '\'' || c == '+' || c == ':'
                             then txt <> "?" <> T.singleton c else txt <> T.singleton c) ""

checkSize :: T.Text -> DEType -> Int -> Maybe Int -> T.Text -> Either T.Text T.Text
checkSize key tp minSz maxSz val = go tp minSz maxSz
  where
    len = T.length val

    go _   _      (Just maxSz') | len > maxSz' = Left ("Field '" <> key <> "' has a maxsize of " <> T.pack (show maxSz') <>
                                                       " but provided value '" <> val <> "' has a length of " <> T.pack (show len))
    go Num minSz' _             | len < minSz' = Right (T.replicate (minSz'-len) "0" <> val)
    go _   minSz' _             | len < minSz' = Left ("Field '" <> key <> "' has a minsize of " <> T.pack (show minSz') <>
                                                      " but provided value '" <> val <> "' has a length of " <> T.pack (show len))
    go _   _      _                            = Right val

fillDe :: M.Map T.Text T.Text -> T.Text -> DE -> Either T.Text DEValue
fillDe _        _      (DEval v)                     = Right v
fillDe userVals prefix (DEdef deNm deTp minSz maxSz _ _ valids) =
  let key = if T.null prefix then deNm else prefix <> "." <> deNm
      mval = M.lookup key userVals
  in case mval of
    Nothing -> Left $ "Required key '" <> key <> "' missing in userVals"
    (Just val) -> if not (isJust valids) || val `elem` (fromJust valids)
                  then if isBinaryType deTp
                       then Right (DEBinary (TE.encodeUtf8 val)) -- FIXME should already receive bytestrings
                       else (escape <$> checkSize key deTp minSz maxSz val) >>= Right . DEStr
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
