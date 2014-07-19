{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.HbciDef where

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Traversable (traverse)
import           Data.Maybe (catMaybes, listToMaybe)
import           Control.Applicative ((<$>), (<|>), (<*>))
import           System.IO (openFile, hClose, IOMode(..))
import           Text.XML.Light
import           Control.Monad (when)

import           Data.HBCI.Types

findAttrByKey :: String -> [Attr] -> Maybe String
findAttrByKey k attr = attrVal <$> L.find (\a -> qName (attrKey a) == k) attr

{-
setDEGItem :: (DEGItem -> DEGItem) -> [T.Text] -> DEGItem -> DEGItem
setDEGItem f [nm]      de@(DE deNm _ _ _ _ _ _)              = if nm == deNm then f de else de
setDEGItem f (nm:nms) deg@(DEG degNm _ _ (DEGdef items)) =
  if nm /= degNm then deg
  else deg { degDef = DEGdef (map (setDEGItem f nms) items) }
setDEGItem _ _ v                                             = v

setDEGValids :: T.Text -> [T.Text] -> DEGItem -> DEGItem
setDEGValids nm valids = setDEGItem (\de -> de { deValids = Just valids }) (T.split (== '.') nm)

setDEGValue :: T.Text -> T.Text -> DEGItem -> DEGItem
setDEGValue nm val = setDEGItem (\_ -> DEVal (DEStr val)) (T.split (== '.') nm)

setSFItem :: (DEGItem -> DEGItem) -> [T.Text] -> SFItem -> SFItem
setSFItem f (nm:nms) seg@(SEG segNm _ _ (SEGdef tag items)) =
  if nm == segNm then seg { sfiSegDef = SEGdef tag (map (setDEGItem f nms) items) } else seg
setSFItem f (nm:nms)  sf@(SF sfNm _ _ (SFdef items)) =
  if nm == sfNm then sf { sfiSfDef = SFdef (map (setSFItem f nms) items) } else sf
setSFItem _ _       x = x

setSFValue :: T.Text -> T.Text -> SFItem -> SFItem
setSFValue nm val = setSFItem (\_ -> DEVal (DEStr val)) (T.split (== '.') nm)
-}
-- setDE f (nm:nms) deg@(DEG degNm _ _ (DEGdef items)) =
--   if nm /= degNm then deg
--   else deg { degDef = DEGdef (map (setDEGItem f nms) items) }

setDE :: (DE -> DE) -> [T.Text] -> DE -> DE
setDE f nms de@(DEdef deNm _ _ _ _ _ _) = if nms == (T.split (== '.') deNm) then f de else de
setDE _ _   v                        = v

setSEGItem :: (DE -> DE) -> [T.Text] -> SEGItem -> SEGItem
setSEGItem f nms (DEItem de) = DEItem (setDE f nms de)
setSEGItem f nms (DEGItem deg@(DEG degNm minnum maxnum des)) =
  let degNms           = if T.null degNm then [] else T.split (== '.') degNm
      n                = length degNms
      (nmsPre,nmsSuff) = splitAt n nms
  in if degNms == nmsPre
     then (DEGItem (DEG degNm minnum maxnum (map (setDE f nmsSuff) des)))
     else (DEGItem deg)

setValids :: ((DE -> DE) -> [T.Text] -> a -> a) -> T.Text -> [T.Text] -> a -> a
setValids f nm valids = f (\de -> de { deValids = Just valids }) (T.split (== '.') nm)

setValue :: ((DE -> DE) -> [T.Text] -> a -> a) -> T.Text -> T.Text -> a -> a
setValue f nm val = f (\_ -> DEval (DEStr val)) (T.split (== '.') nm)

setDEValids :: T.Text -> [T.Text] -> DE -> DE
setDEValids = setValids setDE

setDEValue :: T.Text -> T.Text -> DE -> DE
setDEValue = setValue setDE

setSEGValids :: T.Text -> [T.Text] -> SEGItem -> SEGItem
setSEGValids = setValids setSEGItem

setSEGValue :: T.Text -> T.Text -> SEGItem -> SEGItem
setSEGValue = setValue setSEGItem

setSF :: (DE -> DE) -> [T.Text] -> SF -> SF
setSF f nms sf@(SF _ _ items) = sf { sfItems = map go items }
  where
    go seg@(SEG segNm tag items') =
      let segNms           = if T.null segNm then [] else T.split (== '.') segNm
          n                = length segNms
          (nmsPre,nmsSuff) = splitAt n nms
      in if segNms == nmsPre then SEG segNm tag (map (setSEGItem f nmsSuff) items') else seg

setSFValue :: T.Text -> T.Text -> SF -> SF
setSFValue = setValue setSF

elemToDE :: Element -> Maybe DE
elemToDE (Element nm attrs _ _) = do
    when (qName nm /= "DE") Nothing
    name <- T.pack <$> findAttrByKey "name" attrs
    tp <- fst <$> (listToMaybe . reads =<< findAttrByKey "type" attrs)
    let minSz  = maybe 0  id $ (fst <$> (listToMaybe . reads =<< findAttrByKey "minsize" attrs ))
        maxSz  = fst <$> (listToMaybe . reads =<< findAttrByKey "maxsize" attrs)
        minNum = maybe 1 read (findAttrByKey "minnum" attrs)
        maxNum = findAttrByKey "maxnum" attrs >>= \x -> if x == "0" then Nothing else Just (read x)
    return $ DEdef name tp minSz maxSz minNum maxNum Nothing

elemToValids :: Element -> Maybe (T.Text, [T.Text])
elemToValids (Element nm attrs ctnt _) = do
  when (qName nm /= "valids") Nothing
  path <- findAttrByKey "path" attrs
  return (T.pack path, T.pack . strContent <$> onlyElems ctnt)

elemToValue :: Element -> Maybe (T.Text, T.Text)
elemToValue e@(Element nm attrs _ _) = do
  when (qName nm /= "value") Nothing
  path <- findAttrByKey "path" attrs
  return (T.pack path, T.pack $ strContent e)

getDEGType :: Element -> Maybe T.Text
getDEGType (Element nm attrs _ _) = do
  when (qName nm /= "DEG") Nothing
  T.pack <$> findAttrByKey "type" attrs

-- FIXME: Some DEG references in xml set names and minnums ... thats
-- somewhat confusing - maybe I need to respect that when splicing
-- things in (?)
elemToDEG :: M.Map T.Text DEG -> Element -> Maybe (T.Text, DEG)
elemToDEG degs (Element nm attrs ctnt _) = do
    when (qName nm /= "DEGdef") Nothing
    degid <- T.pack <$> findAttrByKey "id" attrs
    items <- L.foldl' f (Just []) (onlyElems ctnt)
    return (degid, DEG "" 0 Nothing (reverse items))
  where
    f (Just items) e = ((:items) <$> elemToDE e) <|>
                       (getDEGType e >>= \id_ -> M.lookup id_ degs >>= \deg -> return (degItems deg ++ items)) <|>
                       ((\(x,y) -> map (setDEValids x y) items) <$> elemToValids e) <|>
                       ((\(x,y) -> map (setDEValue x y) items) <$> elemToValue e)
    f _ _            = Nothing

findRequestTag :: [Attr] -> Bool
findRequestTag = maybe False (== "1") . findAttrByKey "needsRequestTag"

getCommonAttrsWDefault :: [Attr] -> (T.Text, Int, Maybe Int)
getCommonAttrsWDefault attrs = (name, minnum, maxnum)
  where
    name   = maybe "" T.pack (findAttrByKey "name" attrs)
    minnum = maybe 1 read (findAttrByKey "minnum" attrs)
    maxnum = maybe (Just 1) (\x -> let y = read x in if y == 0 then Nothing else Just y) (findAttrByKey "maxnum" attrs)

elemToSEG :: M.Map T.Text DEG -> Element -> Maybe (T.Text, SEG)
elemToSEG degs (Element nm attrs ctnt _) = do
  when (qName nm /= "SEGdef") Nothing
  id_ <- T.pack <$> findAttrByKey "id" attrs
  items <- L.foldl' f (Just []) (onlyElems ctnt)
  return (id_, SEG "" (findRequestTag attrs) (reverse items))
  where
    f :: Maybe [SEGItem] -> Element -> Maybe [SEGItem]
    f (Just items) e = ((:items) . DEItem <$> elemToDE e) <|>
                       (let attrs                = elAttribs e
                            (nm, minnum, maxnum) = getCommonAttrsWDefault attrs
                        in do tp <- T.pack <$> findAttrByKey "type" attrs
                              (DEG _ _ _ des) <- M.lookup tp degs
                              return (DEGItem (DEG nm minnum maxnum des) : items)) <|>
                       ((\(x,y) -> map (setSEGValids x y) items) <$> elemToValids e) <|>
                       ((\(x,y) -> map (setSEGValue x y) items) <$> elemToValue e)
    f _ _            = Nothing

-- The way SFs and SEGs are represented is not really consistent with
-- the XML representation. In particular, SFs can be nested inside the
-- XML and each SF can have a minnum and a maxnum specified. This way
-- the XML representation can describe if a group of SEGs is
-- required. For example imagine an SF, some SEGs are required, some
-- are not. If this SF is included somewhere with a minnum of 0, then
-- the logical interpretation is if one of the SEGs inside this SF is
-- present then all the SEGs inside this SF with a minnum > 0 are
-- required. This could be logically represented by having minnums and
-- maxnums both on SFs and SEGs.
--
-- The representation of SFs and SEGs in the XML allows considerable
-- more complex constructs. For example, one could represent an SF
-- with arbitrary deep nesting. However, this is the not really needed
-- by HBCI.
--
-- For now I have chosen a really simple representation which might be
-- to simple in some cases. minums and maxnums are only available on
-- SFs and not on SEGs. SEGs are represented as SFs with a single item
-- and the minnums and maxnums are relaxed when an SF is referenced.
-- This might be too simplistic in some cases, I'll fix that if it
-- turns out to be the case.
elemToSFItem :: M.Map T.Text SEG -> M.Map T.Text [SF] -> Element -> Maybe [SF]
elemToSFItem segs sfs (Element nm attrs _ _) = do
  type_ <- T.pack <$> findAttrByKey "type" attrs
  let (name, minnum, maxnum) = getCommonAttrsWDefault attrs
  if (qName nm == "SF")
  then M.lookup type_ sfs >>= return . map (updateMinMax minnum maxnum)
  else if (qName nm == "SEG")
       then M.lookup type_ segs >>= \(SEG _ tag items) -> return $ [SF minnum maxnum [SEG name tag items]]
       else Nothing
  where
    updateMinMax outerMin outerMax (SF innerMin innerMax items) =
      let newMin = min innerMin outerMin
          newMax = max <$> innerMax <*> outerMax
      in SF newMin newMax  items

elemToSF :: M.Map T.Text SEG -> M.Map T.Text [SF] -> Element -> Maybe (T.Text, [SF])
elemToSF segs sfs (Element nm attrs ctnt _) =  do
  when (qName nm /= "SFdef") Nothing
  id_ <- T.pack <$> findAttrByKey "id" attrs
  items <- concat <$> traverse (elemToSFItem segs sfs) (onlyElems ctnt)
  return $ (id_, items)

elemToMSG :: M.Map T.Text SEG -> M.Map T.Text [SF] -> Element -> Maybe (T.Text, MSG)
elemToMSG segs sfs (Element nm attrs ctnt _) = do
  when (qName nm /= "MSGdef") Nothing
  id_ <- T.pack <$> findAttrByKey "id" attrs
  let reqSig   = maybe True (/="1") $ findAttrByKey "dontsign" attrs
      reqCrypt = maybe True (/="1") $ findAttrByKey "dontcrypt" attrs
  items <- L.foldl' f (Just []) (onlyElems ctnt)
  return $ (id_, MSG reqSig reqCrypt (reverse items))
  where
    f (Just items) e = ((++ items) <$> elemToSFItem segs sfs e) <|>
                       ((\(x,y) -> map (setSFValue x y) items) <$> elemToValue e)
    f _ _            = Nothing

getXml :: String -> IO [Content]
getXml fname = do
  h <- openFile fname ReadMode
  xmlData <- BS.hGetContents h
  hClose h
  return $ parseXML xmlData

getChildrenByName :: String -> [Content] -> [Element]
getChildrenByName name ctnts = onlyElems ctnts >>= filterChildrenName (\x -> name == qName x) >>= elChildren

getDEGs :: [Content] -> M.Map T.Text DEG
getDEGs = L.foldl' f M.empty . getChildrenByName "DEGs"
  where
    f degs e = maybe degs (\(id_, deg) -> M.insert id_ deg degs) $ elemToDEG degs e

getSEGs :: M.Map T.Text DEG -> [Content] -> M.Map T.Text SEG
getSEGs degs = M.fromList . catMaybes . map (elemToSEG degs) . getChildrenByName "SEGs"

getSFs :: M.Map T.Text SEG -> [Content] -> M.Map T.Text [SF]
getSFs segs = L.foldl' f M.empty . getChildrenByName "SFs"
  where
    f sfs e = maybe sfs (\(id_, sf) -> M.insert id_ sf sfs) $ elemToSF segs sfs e

getMSGs :: M.Map T.Text SEG -> M.Map T.Text [SF] -> [Content] -> M.Map T.Text MSG
getMSGs segs sfs = M.fromList . catMaybes . map (elemToMSG segs sfs) . getChildrenByName "MSGs"

getMSGfromXML :: [Content] -> M.Map T.Text MSG
getMSGfromXML xml =
    let degs = getDEGs xml
        segs = getSEGs degs xml
        sfs  = getSFs segs xml
    in getMSGs segs sfs xml
