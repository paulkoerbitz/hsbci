{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.HBCI.Messages where

import           Control.Applicative ((<$>))
import           Control.Arrow (second)
import           Control.Monad (foldM)
import           Control.Monad.State (StateT, evalStateT, get, modify)
import           Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import           Data.Either (partitionEithers, rights)
import           Data.Monoid ((<>))
import qualified Data.Map  as M
import           Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
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

fillDe :: Maybe DEValue -> DE -> FillRes DEValue
fillDe _ (DEval v)                                = updateSize $! v
fillDe _ (DEdef deNm _ _ _ _ _ _) | deNm == "seq" = do
  MkFillState _ seqNum  <- get
  updateSize $! DEStr $! T.pack $! show seqNum
fillDe Nothing (DEdef deNm _ _ _ minNum _ _)    = if minNum == 0 then return (DEStr "") else lift $ Left $ "Required deName '" <> deNm <> "' missing in entries"
fillDe (Just val) (DEdef deNm deTp minSz maxSz _ _ valids) =
  case (isBinaryType deTp, val) of
    (True,  (DEBinary b)) -> return (DEBinary b)
    (True,  _           ) -> lift $! Left $! "Value for DE " <> deNm <> " must be binary"
    (False, (DEStr s))    -> if not (isJust valids) || s `elem` (fromJust valids)
                             then (escape <$> lift (checkSize deNm deTp minSz maxSz s)) >>= updateSize . DEStr
                             else lift $! Left $! "Value '" <> s <> "' for deName '" <> deNm <> "' not in valid values '" <> T.pack (show (fromJust valids)) <> "'"
    (False, _           ) -> lift $! Left $! "Value for DE " <> deNm <> " must not be binary"


fillSegItem :: SEGEntry -> SEGItem -> FillRes DEGValue
fillSegItem entries (DEItem de)                             =
  case M.lookup (deName de) entries of
    Just (DEGentry _)      -> lift $! Left $! "Expected 'DEentry' for DE '" <> deName de <> "' but found DEGentry"
    Nothing                -> (:[]) <$> fillDe Nothing de
    Just (DEentry deEntry) -> (:[]) <$> fillDe (Just deEntry) de
fillSegItem entries (DEGItem (DEG degnm minnum _ degitems)) =
  case M.lookup degnm entries of
    Nothing -> if minnum == 0 then return [] else lift $! Left $! "No entries for required DEG '" <> degnm <> "' found"
    Just (DEentry _) -> lift $! Left $! "Expected 'DEGentry' for DEG '" <> degnm <> "' but found DEentry"
    Just (DEGentry degentries) -> do
      modify (\x -> x { msgSize = msgSize x + max (length degitems - 1) 0 })
      traverse (\de -> fillDe (M.lookup (deName de) degentries) de) degitems

fillSeg :: MSGEntry -> SEG -> FillRes SEGValue
fillSeg entries (SEG segNm _tag minnum _ items) =
  case M.lookup segNm entries of
    Nothing -> if minnum == 0 then return [] else lift $! Left $! "No entries for required SEG '" <> segNm <> "' found"
    Just segEntries -> do
      res <- traverse (fillSegItem segEntries) items
      -- msgSize: length items - 1 (for the + in between items) + 1 (for the ' after the seg)
      modify (\x -> x { msgSeq = msgSeq x + 1 , msgSize = msgSize x + length items})
      return res

fillMsg :: MSGEntry -> MSG -> Either T.Text MSGValue
fillMsg entries (MSG _reqSig _reqEnc items) =
  evalStateT (traverse (fillSeg entries') items >>= replaceMsgSize) (MkFillState 0 1)
  where
    replaceMsgSize ((head:[DEStr "000000000000"]:xs):ys) = do
      MkFillState sz _ <- get
      return ((head:[DEStr (T.justifyRight 12 '0' $ T.pack (show sz))]:xs):ys)
    replaceMsgSize _ = lift $! Left "Didn't find expected field message size"

    entries' = M.insertWith M.union "MsgHead" (M.fromList [("msgsize", DEentry $ DEStr "000000000000")]) entries

    -- This is a hack that is really not very pretty - the whole thing should
    -- really be properly refactored
    -- fillSf :: SEG -> FillRes MSGValue
    -- fillSf (SEG nm tag minnum maxnum items) = do
    --   state <- get
    --   case runStateT (traverse (fillSeg userVals') items) state of
    --     Left err  -> if minnum == 0 then return [] else lift (Left err)
    --     Right (res, state') -> (put state' >> lift (Right res))


-- FIXME: A use case for lenses(?)
getValHead :: SEGValue -> Either T.Text (T.Text, T.Text)
getValHead ((DEStr hd:_:DEStr vers:_):_) = Right (hd, vers)
getValHead _                             = Left "Required element MsgHead not found"

getDefHead :: SEG -> Either T.Text (T.Text, T.Text)
getDefHead (SEG _ _ _ _ (DEGItem (DEG _ _ _ (DEval (DEStr hd):_:DEval (DEStr vers):_)):_)) = Right (hd, vers)
getDefHead _                                                                               = Left "getDefHead: head not found"

checkMinnum :: Int -> T.Text -> MSGValue -> Either T.Text (MSGValue, [a])
checkMinnum minnum segNm vals = if minnum > 0
                                then Left $ "Required SEG '" <> segNm <> "' not found"
                                else Right (vals, [])

validateAndExtractSegItem :: T.Text -> SEGItem -> DEGValue -> Either T.Text [(T.Text, DEValue)]
validateAndExtractSegItem _      (DEItem (DEval deVal)) [deVal'] | deVal == deVal' =
  return []
validateAndExtractSegItem prefix (DEItem (DEval deVal)) [deVal']                   =
  Left $ "Unexpected value in segment " <> prefix <> ", expected '" <> T.pack (show deVal) <> "' but got '" <> T.pack (show deVal') <> "'"
validateAndExtractSegItem prefix (DEItem (DEdef nm _tp _minSz _maxSz _minNum _maxNum _valids)) [deVal] =
  return [(concatPrefix prefix nm, deVal)] -- FIXME: validate
validateAndExtractSegItem prefix (DEItem (DEdef nm _tp minSz _maxSz minNum _maxNum _valids)) [] =
  let prefix' = concatPrefix prefix nm
  in if minNum == 0 then return []
     else if minSz == 0 then return [(prefix', DEStr "")]
          else Left $ "Required DE '" <> prefix' <> "' not found during extraction"
validateAndExtractSegItem prefix (DEGItem (DEG degNm _minNum _maxNum des)) devals    =
  -- This is really a hack to use the above implementations - not very nice
  let prefix' = concatPrefix prefix degNm
      deitems = map DEItem des
      devals' = map (:[]) devals
      items   = zip deitems devals'
  in concat <$> mapM (uncurry $ validateAndExtractSegItem prefix') items
validateAndExtractSegItem prefix _ _ = Left $ "Unexpected deval when trying to process segment '" <> prefix <> "'"

extractSeg :: M.Map T.Text SEG -> SEGValue -> Either T.Text [(T.Text, DEValue)]
extractSeg defs segVal = do
  valHd <- getValHead segVal
  case M.lookup (fst valHd <> "-" <> snd valHd) defs of
    Nothing -> Left $ "No definition for seg head " <> fst valHd <> "-" <> snd valHd
    Just segDef -> let items = segItems segDef
                       prefix = segName segDef
                   in foldM (\acc (si,sv) -> (++ acc) <$> validateAndExtractSegItem prefix si sv) [] $ zip items segVal

findSegDefs :: MSG -> M.Map T.Text SEG
findSegDefs (MSG _ _ segs) = M.fromList $! rights $! f <$> segs
  where
    f seg = case getDefHead seg of
      Right (hd, version) -> Right (hd <> "-" <> version, seg)
      Left x              -> Left x

extractMsg :: MSG -> MSGValue -> ([T.Text], [(T.Text, DEValue)])
extractMsg msgDef msgVal =
  second concat $ partitionEithers $ map (extractSeg $ findSegDefs msgDef) msgVal
