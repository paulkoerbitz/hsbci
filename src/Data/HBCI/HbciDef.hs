{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.HbciDef where

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Maybe (catMaybes, listToMaybe)
import           Control.Applicative ((<$>), (<|>))
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

{-
elemToSFItem :: M.Map T.Text SEGdef -> M.Map T.Text SFdef -> Element -> Maybe SFItem
elemToSFItem segs sfs (Element nm attrs _ _) = do
  type_ <- T.pack <$> findAttrByKey "type" attrs
  let (name, minnum, maxnum) = getCommonAttrsWDefault attrs
  if (qName nm == "SF")
  then M.lookup type_ sfs >>= \sf -> return $ SF name minnum maxnum sf
  else if (qName nm == "SEG")
       then M.lookup type_ segs >>= \seg -> return $ SEG name minnum maxnum seg
       else Nothing

elemToSFdef :: M.Map T.Text SEGdef -> M.Map T.Text SFdef -> Element -> Maybe (T.Text, SFdef)
elemToSFdef segs sfs (Element nm attrs ctnt _) = do
  when (qName nm /= "SFdef") Nothing
  id_ <- T.pack <$> findAttrByKey "id" attrs
  items <- sequence . map (elemToSFItem segs sfs) $ onlyElems ctnt
  return $ (id_, SFdef items)

elemToMSGdef :: M.Map T.Text SEGdef -> M.Map T.Text SFdef -> Element -> Maybe (T.Text, MSGdef)
elemToMSGdef segs sfs (Element nm attrs ctnt _) = do
  when (qName nm /= "MSGdef") Nothing
  id_ <- T.pack <$> findAttrByKey "id" attrs
  let reqSig   = maybe True (/="1") $ findAttrByKey "dontsign" attrs
      reqCrypt = maybe True (/="1") $ findAttrByKey "dontcrypt" attrs
  items <- L.foldl' f (Just []) (onlyElems ctnt)
  return $ (id_, MSGdef reqSig reqCrypt items)
  where
    f (Just items) e = ((:items) <$> elemToSFItem segs sfs e) <|>
                       ((\(x,y) -> map (setSFValue x y) items) <$> elemToValue e)
    f _ _            = Nothing
-}

getXml :: String -> IO [Content]
getXml fname = do
  h <- openFile fname ReadMode
  xmlData <- BS.hGetContents h
  hClose h
  return $ parseXML xmlData

getChildrenByName :: String -> [Content] -> [Element]
getChildrenByName name ctnts = onlyElems ctnts >>= filterChildrenName (\x -> name == qName x) >>= elChildren

getDEGs :: [Content] -> M.Map T.Text DEG
getDEGs = undefined -- M.fromList . catMaybes . map elemToDEG . getChildrenByName "DEGs"

getSEGs :: M.Map T.Text DEG -> [Content] -> M.Map T.Text SEG
getSEGs degs = M.fromList . catMaybes . map (elemToSEG degs) . getChildrenByName "SEGs"

{-
getSFdefs :: M.Map T.Text SEGdef -> [Content] -> M.Map T.Text SFdef
getSFdefs segs = L.foldl' f M.empty . getChildrenByName "SFs"
  where
    f sfs e = maybe sfs (\(id_, sf) -> M.insert id_ sf sfs) $ elemToSFdef segs sfs e

getMSGdefs :: M.Map T.Text SEGdef -> M.Map T.Text SFdef -> [Content] -> M.Map T.Text MSGdef
getMSGdefs segs sfs = M.fromList . catMaybes . map (elemToMSGdef segs sfs) . getChildrenByName "MSGs"

getMSGs :: [Content] -> M.Map T.Text MSGdef
getMSGs xml =
    let degs = getDEGdefs xml
        segs = getSEGdefs degs xml
        sfs  = getSFdefs segs xml
    in getMSGdefs segs sfs xml
-}
