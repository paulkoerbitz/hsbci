{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.HBCI.Messages where

import           Control.Applicative ((<$>))
import           Control.Monad (foldM)
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

import           Debug.Trace

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

updateSize :: DEValue -> FillRes DEValue
updateSize (DEStr v)    = modify (\x -> x { msgSize = msgSize x + T.length v }) >> lift (Right (DEStr v))
updateSize (DEBinary b) = let lengthBody   = BS.length b
                              lengthHeader = 2 + length (show lengthBody)
                          in modify (\x -> x { msgSize = msgSize x + lengthBody + lengthHeader }) >> lift (Right (DEBinary b))

fillDe :: M.Map T.Text T.Text -> T.Text -> DE -> FillRes DEValue
fillDe _        _      (DEval v)                                = updateSize $! v
fillDe _        _      (DEdef deNm _ _ _ _ _ _) | deNm == "seq" = do
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
  -- msgSize: length items - 1 (for the + in between items) + 1 (for the ' after the seg)
  modify (\x -> x { msgSeq = msgSeq x + 1 , msgSize = msgSize x + length items})
  return res

fillSegItem :: M.Map T.Text T.Text -> T.Text -> SEGItem -> FillRes DEGValue
fillSegItem userVals = go
  where
    go prefix (DEItem de)                        = (:[]) <$> fillDe userVals prefix de
    go prefix (DEGItem (DEG degnm _ _ degitems)) =
      let newPrefix = if T.null degnm then prefix else prefix <> "." <> degnm
          n         = max (length degitems - 1) 0 -- number of : between DEs
      in do
        modify (\x -> x { msgSize = msgSize x + n })
        traverse (fillDe userVals newPrefix) degitems

fillMsg :: M.Map T.Text T.Text -> MSG -> Either T.Text MSGValue
fillMsg userVals (MSG _reqSig _reqEnc items) =
  evalStateT ((concat <$> traverse fillSf items) >>= replaceMsgSize) (MkFillState 0 1)
  where
    replaceMsgSize ((head:[DEStr "000000000000"]:xs):ys) = do
      MkFillState sz _ <- get
      return ((head:[DEStr (T.justifyRight 12 '0' $ T.pack (show sz))]:xs):ys)
    replaceMsgSize _ = lift $! Left "Didn't find expected field message size"

    userVals' = M.insert "MsgHead.msgsize" "000000000000" userVals

    fillSf :: SF -> FillRes MSGValue
    fillSf (SF _ _ items) = traverse (fillSeg userVals') items

-- FIXME: A use case for lenses(?)
getValHead :: SEGValue -> Either T.Text T.Text
getValHead ((DEStr hd:_):_) = Right hd
getValHead _                = Left "Required element MsgHead not found"

getDefHead :: SEG -> Either T.Text T.Text
getDefHead (SEG _ _ (DEGItem (DEG _ _ _ (DEval (DEStr hd):_)):_)) = Right hd
getDefHead _                                                      = Left "getDefHead: head not found"

checkMinnum :: Int -> T.Text -> MSGValue -> Either T.Text (MSGValue, [(T.Text,T.Text)])
checkMinnum minnum segNm vals = if minnum > 0
                                then Left $ "Required SEG '" <> segNm <> "' not found"
                                else Right (vals, [])

-- data ExtractErr = ExtractNotFound T.Text
--                 | ExtractError T.Text
--
-- validateAndExtractSeg :: SEG -> SEGValue -> Either ExtractErr (SEGValue, M.Map T.Text T.Text)
-- validateAndExtractSeg = undefined
--
-- validateAndExtractSf :: SF -> MSGValue -> Either T.Text (MSGValue, M.Map T.Text T.Text)
-- validateAndExtractSf (SF minnum _ (segDef:segDefs)) (segVal:segVals) =
--   case validateAndExtractSeg segDef segVal of
--     Left (ExtractError err) -> Left err
--     Left (ExtractNotFound err) -> if minnum > 0 then Left err else Right ((segVal:segVals), M.empty)
--     Right (val, map) -> undefined -- go on

validateAndExtractSegItem :: SEGItem -> DEGValue -> Either T.Text [(T.Text, T.Text)]
validateAndExtractSegItem = undefined

validateAndExtractSeg :: SF -> MSGValue -> Either T.Text (MSGValue, [(T.Text,T.Text)])
validateAndExtractSeg (SF minnum _ (seg:_))    []   = checkMinnum minnum (segName seg) []
validateAndExtractSeg (SF _      _ [])         vals = return (vals, [])
validateAndExtractSeg (SF minnum maxnum (seg:segs)) (segVal:segVals) = do
  valHd <- getValHead segVal
  defHd <- getDefHead seg
  if valHd == defHd
    then let items = segItems seg
         in do vals <- foldM (\acc (si,sv) -> (++ acc) <$> validateAndExtractSegItem si sv) [] $ zip items segVal
               (segVals', otherVals) <- validateAndExtractSeg (SF minnum maxnum segs) segVals
               return (segVals', vals ++ otherVals)
    else checkMinnum minnum (segName seg) (segVal:segVals)

validateAndExtract :: MSG -> MSGValue -> Either T.Text (M.Map T.Text T.Text)
validateAndExtract (MSG _ _ sfs) msg = M.fromList <$> go [] msg sfs
  where
    -- So the message has one level less than the SFs.
    -- However, for each SF we might already consume SEGs
    -- Then there are optional sfs and segvals.
    -- So sometimes I'll have to throw away a definition
    -- So this is fundamentally an applicative problem:
    -- the results for the current analysis don't depend on
    -- the results of the previous step, they just depend
    -- on the side effects. As such I should be able to use
    -- something applicatively
    -- But how do I build this applicative? With monads I
    -- can take for example the state monad and use that to
    -- update state - with applicative I could imagine that the
    -- respective data structures already have been there.
    --
    -- How can I get started? The result is Either T.Text (M.Map T.Text T.Text)
    -- one way to think about it could be bottom up as well:
    -- Every SEG has an identifier, so I can check if this
    -- identifier matches or not and move on accordingly
    go vals segVals sfs               = undefined -- goSeg vals sfs segVals >>= \val -> go (val:vals) sfs segVals
    go vals []      []                = Right vals
    go _    _       _                 = Left "Invalid message"

    goSeg vals _ _                    = Left "unimplemented"
