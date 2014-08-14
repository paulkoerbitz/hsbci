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

concatPrefix :: T.Text -> T.Text -> T.Text
concatPrefix prefix suffix | T.null prefix = suffix
concatPrefix prefix suffix | T.null suffix = prefix
concatPrefix prefix suffix                 = prefix <> "." <> suffix

data FillState = MkFillState { msgSize :: !Int, msgSeq :: !Int }

type FillRes = StateT FillState (Either T.Text)

updateSize :: DEValue -> FillRes DEValue
updateSize (DEStr v)    = modify (\x -> x { msgSize = msgSize x + T.length v }) >> lift (Right (DEStr v))
updateSize (DEBinary b) = let lengthBody   = BS.length b
                              lengthHeader = 2 + length (show lengthBody)
                          in modify (\x -> x { msgSize = msgSize x + lengthBody + lengthHeader }) >> lift (Right (DEBinary b))

fillDe :: M.Map T.Text DEValue -> T.Text -> DE -> FillRes DEValue
fillDe _        _      (DEval v)                                = updateSize $! v
fillDe _        _      (DEdef deNm _ _ _ _ _ _) | deNm == "seq" = do
  MkFillState _ seqNum  <- get
  updateSize $! DEStr $! T.pack $! show seqNum
fillDe userVals prefix (DEdef deNm deTp minSz maxSz minNum _ valids) =
  let key = concatPrefix prefix deNm
      mval = M.lookup key userVals
  in case (isBinaryType deTp, mval) of
    (_, Nothing) -> if minNum == 0 then return (DEStr "") else lift $ Left $ "Required key '" <> key <> "' missing in userVals"
    (True,  Just (DEBinary b)) -> return (DEBinary b)
    (True,  Just _           ) -> lift $! Left $! "Value for DE " <> deNm <> " must be binary"
    (False, Just (DEStr s))    -> if not (isJust valids) || s `elem` (fromJust valids)
                                  then (escape <$> lift (checkSize key deTp minSz maxSz s)) >>= updateSize . DEStr
                                  else lift $! Left $! "Value '" <> s <> "' for key '" <> key <> "' not in valid values '" <> T.pack (show (fromJust valids)) <> "'"


fillSeg :: M.Map T.Text DEValue -> SEG -> FillRes SEGValue
fillSeg userVals (SEG segNm _ items) = do
  res <- traverse (fillSegItem userVals segNm) items
  -- msgSize: length items - 1 (for the + in between items) + 1 (for the ' after the seg)
  modify (\x -> x { msgSeq = msgSeq x + 1 , msgSize = msgSize x + length items})
  return res

fillSegItem :: M.Map T.Text DEValue -> T.Text -> SEGItem -> FillRes DEGValue
fillSegItem userVals = go
  where
    go prefix (DEItem de)                        = (:[]) <$> fillDe userVals prefix de
    go prefix (DEGItem (DEG degnm _ _ degitems)) =
      let newPrefix = concatPrefix prefix degnm
          n         = max (length degitems - 1) 0 -- number of : between DEs
      in do
        modify (\x -> x { msgSize = msgSize x + n })
        traverse (fillDe userVals newPrefix) degitems

fillMsg :: M.Map T.Text DEValue -> MSG -> Either T.Text MSGValue
fillMsg userVals (MSG _reqSig _reqEnc items) =
  evalStateT ((concat <$> traverse fillSf items) >>= replaceMsgSize) (MkFillState 0 1)
  where
    replaceMsgSize ((head:[DEStr "000000000000"]:xs):ys) = do
      MkFillState sz _ <- get
      return ((head:[DEStr (T.justifyRight 12 '0' $ T.pack (show sz))]:xs):ys)
    replaceMsgSize _ = lift $! Left "Didn't find expected field message size"

    userVals' = M.insert "MsgHead.msgsize" (DEStr "000000000000") userVals

    fillSf :: SF -> FillRes MSGValue
    fillSf (SF _ _ items) = traverse (fillSeg userVals') items

-- FIXME: A use case for lenses(?)
getValHead :: SEGValue -> Either T.Text (T.Text, T.Text)
getValHead ((DEStr hd:_:DEStr vers:_):_) = Right (hd, vers)
getValHead _                             = Left "Required element MsgHead not found"

getDefHead :: SEG -> Either T.Text (T.Text, T.Text)
getDefHead (SEG _ _ (DEGItem (DEG _ _ _ (DEval (DEStr hd):_:DEval (DEStr vers):_)):_)) = Right (hd, vers)
getDefHead _                                                                           = Left "getDefHead: head not found"

checkMinnum :: Int -> T.Text -> MSGValue -> Either T.Text (MSGValue, [a])
checkMinnum minnum segNm vals = if minnum > 0
                                then Left $ "Required SEG '" <> segNm <> "' not found"
                                else Right (vals, [])

validateAndExtractSegItem :: T.Text -> SEGItem -> DEGValue -> Either T.Text [(T.Text, DEValue)]
validateAndExtractSegItem _      (DEItem (DEval deVal)) [deVal'] | deVal == deVal' =
  return []
validateAndExtractSegItem prefix (DEItem (DEval deVal)) [deVal']                   =
  Left $ "Unexpected value in segment " <> prefix <> ", expected '" <> T.pack (show deVal) <> "' but got '" <> T.pack (show deVal') <> "'"
validateAndExtractSegItem prefix (DEItem (DEdef nm tp minSz maxSz minNum maxNum valids)) [deVal] =
  return [(concatPrefix prefix nm, deVal)] -- FIXME: validate
validateAndExtractSegItem prefix (DEItem (DEdef nm tp minSz maxSz minNum maxNum valids)) [] =
  let prefix' = concatPrefix prefix nm
  in if minNum == 0 then return []
     else if minSz == 0 then return [(prefix', DEStr "")]
          else Left $ "Required DE '" <> prefix' <> "' not found during extraction"
validateAndExtractSegItem prefix (DEGItem (DEG degNm minNum maxNum des)) devals    =
  -- This is really a hack to use the above implementations - not very nice
  let prefix' = concatPrefix prefix degNm
      deitems = map DEItem des
      devals' = map (:[]) devals
      items   = zip deitems devals'
  in concat <$> mapM (uncurry $ validateAndExtractSegItem prefix') items
validateAndExtractSegItem prefix _ _ = Left $ "Unexpected deval when trying to process segment '" <> prefix <> "'"


validateAndExtractSeg :: SF -> MSGValue -> Either T.Text (MSGValue, [(T.Text,DEValue)])
validateAndExtractSeg (SF minnum _ (seg:_))         []               = checkMinnum minnum (segName seg) []
validateAndExtractSeg (SF _      _ [])              vals             = return (vals, [])
validateAndExtractSeg (SF minnum maxnum (seg:segs)) (segVal:segVals) = do
  valHd <- getValHead segVal
  defHd <- getDefHead seg
  if valHd == defHd
    then ("SEG found: defHd=" <> show defHd <> ", valHd=" <> show valHd) `trace`
         let items = segItems seg
             prefix = segName seg
         in do vals <- foldM (\acc (si,sv) -> (++ acc) <$> validateAndExtractSegItem prefix si sv) [] $ zip items segVal
               (segVals', otherVals) <- validateAndExtractSeg (SF minnum maxnum segs) segVals
               return (segVals', vals ++ otherVals)
    else ("SEG not found: defHd=" <> show defHd <> ", valHd=" <> show valHd) `trace` checkMinnum minnum (segName seg) (segVal:segVals)

validateAndExtractSeg' :: M.Map T.Text SEG -> SEGValue -> Either T.Text [(T.Text, DEValue)]
validateAndExtractSeg' defs segVal = do
  valHd <- getValHead segVal
  case M.lookup (fst valHd <> "-" <> snd valHd) defs of
    Nothing -> Left $ "No definition for seg head " <> fst valHd <> "-" <> snd valHd
    Just segDef -> let items = segItems segDef
                       prefix = segName segDef
                   in foldM (\acc (si,sv) -> (++ acc) <$> validateAndExtractSegItem prefix si sv) [] $ zip items segVal

findSEGs :: MSG -> M.Map T.Text SEG
findSEGs = undefined


validateAndExtract :: MSG -> MSGValue -> Either T.Text (M.Map T.Text DEValue)
validateAndExtract (MSG _ _ [])  msgV@(_:_) = Left $ "Could not fully process message: " <> T.pack (show msgV)
validateAndExtract (MSG _ _ sfs) msgV       =
  M.fromList . snd <$> go (msgV, []) sfs
  where
    go result       []       = return result
    go (rest, vals) (sf:sfs) = do
       (rest', vals') <- validateAndExtractSeg sf rest
       if null vals'
         then go (rest', vals) sfs
         else go (rest', vals' ++ vals) (decNums sf:sfs)

    decNums (SF minnum maxnum segs) = let minnum' = if minnum > 0 then minnum - 1 else minnum
                                          maxnum' = if isJust maxnum && fromJust maxnum > 0 then (\x -> x-1) <$> maxnum else maxnum
                                      in (SF minnum' maxnum' segs)
