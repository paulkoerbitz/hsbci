{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.HBCI.Messages where

import           Control.Applicative ((<$>))
import           Control.Monad.State (StateT, evalStateT, get, modify)
import           Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
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

data FillState = MkFillState { msgSize :: !Int, msgSeq :: !Int }

type FillRes = StateT FillState (Either T.Text)

-- FIXME: correct length for binary
updateSize :: DEValue -> FillRes DEValue
updateSize (DEStr v)    = modify (\x -> x { msgSize = msgSize x + T.length v }) >> lift (Right (DEStr v))
updateSize (DEBinary b) = modify (\x -> x { msgSize = msgSize x + BS.length b + 2 }) >> lift (Right (DEBinary b))

-- FIXME: the 'fill' functions should operate on a state monad which
--        maintains the 'size' of the message and the seq of the current
--        segment. fillDe should recognize segments and fill in the number
fillDe :: M.Map T.Text T.Text -> T.Text -> DE -> FillRes DEValue
fillDe _        _      (DEval v)                     = return $! v
fillDe userVals prefix (DEdef deNm _ _ _ _ _ _) | deNm == "seq" = do
  MkFillState _ seqNum  <- get
  updateSize $! DEStr $! T.pack $! show seqNum
fillDe userVals prefix (DEdef deNm deTp minSz maxSz minNum _ valids) =
  let key = if T.null prefix then deNm else prefix <> "." <> deNm
      mval = M.lookup key userVals
  in case mval of
    Nothing -> if minNum == 0 then return (DEStr "") else lift $ Left $ "Required key '" <> key <> "' missing in userVals"
    (Just val) -> if not (isJust valids) || val `elem` (fromJust valids)
                  then if isBinaryType deTp
                       then return $! DEBinary $! TE.encodeUtf8 val -- FIXME should already receive bytestrings
                       else (escape <$> lift (checkSize key deTp minSz maxSz val)) >>= updateSize . DEStr
                  else lift $! Left $! "Value '" <> val <> "' for key '" <> key <> "' not in valid values '" <> T.pack (show (fromJust valids)) <> "'"

fillSeg :: M.Map T.Text T.Text -> SEG -> FillRes SEGValue
fillSeg userVals (SEG segNm _ items) = do
  res <- traverse (fillSegItem userVals segNm) items
  modify (\x -> x { msgSeq = msgSeq x + 1 })
  return res

fillSegItem :: M.Map T.Text T.Text -> T.Text -> SEGItem -> FillRes DEGValue
fillSegItem userVals = go
  where
    go prefix (DEItem de)                        = (:[]) <$> fillDe userVals prefix de
    go prefix (DEGItem (DEG degnm _ _ degitems)) =
      let newPrefix = if T.null degnm then prefix else prefix <> "." <> degnm
      in traverse (fillDe userVals newPrefix) degitems

fillMsg :: M.Map T.Text T.Text -> MSG -> Either T.Text MSGValue
fillMsg userVals (MSG _reqSig _reqEnc items) =
  evalStateT (concat <$> traverse fillSf items) (MkFillState 0 1)
  where
    -- FIXME: Overwrite this once the full message has been generated
    userVals' = M.insert "MsgHead.msgsize" "000000000000" userVals

    fillSf :: SF -> FillRes MSGValue
    fillSf (SF _ _ items) = traverse (fillSeg userVals') items
