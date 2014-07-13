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

elemToDEdef :: Element -> Maybe DEGItem
elemToDEdef (Element nm attrs _ _) = do
    when (qName nm /= "DE") Nothing
    name <- T.pack <$> findAttrByKey "name" attrs
    tp <- fst <$> (listToMaybe . reads =<< findAttrByKey "type" attrs)
    let minSz  = maybe 0  id $ (fst <$> (listToMaybe . reads =<< findAttrByKey "minsize" attrs ))
        maxSz  = fst <$> (listToMaybe . reads =<< findAttrByKey "maxsize" attrs)
        minNum = maybe 1 read (findAttrByKey "minnum" attrs)
        maxNum = findAttrByKey "maxnum" attrs >>= \x -> if x == "0" then Nothing else Just (read x)
    return $ DE name tp minSz maxSz minNum maxNum Nothing

setDEGItem :: (DEGItem -> DEGItem) -> [T.Text] -> DEGItem -> DEGItem
setDEGItem f [nm]      de@(DE deNm _ _ _ _ _ _)              = if nm == deNm then f de else de
setDEGItem f (nm:nms) deg@(DEG degNm _ _ (DEGdef tag items)) =
  if nm /= degNm then deg
  else deg { degDef = DEGdef tag (map (setDEGItem f nms) items) }
setDEGItem _ _ v                                             = v

setDEGValids :: T.Text -> [T.Text] -> DEGItem -> DEGItem
setDEGValids nm valids = setDEGItem (\de -> de { deValids = Just valids }) (T.split (== '.') nm)

setDEGValue :: T.Text -> T.Text -> DEGItem -> DEGItem
setDEGValue nm val = setDEGItem (\_ -> DEVal (DEStr val)) (T.split (== '.') nm)

setSFItem :: (DEGItem -> DEGItem) -> [T.Text] -> SFItem -> SFItem
setSFItem f (nm:nms) seg@(SEG segNm _ _ (SEGdef tag items)) =
  if nm == segNm then seg { sfiSegDef = SEGdef tag (map (setDEGItem f nms) items) } else seg
setSFItem f (nm:nms)  sf@(SF sfNm _ _ (SFdef tag items)) =
  if nm == sfNm then sf { sfiSfDef = SFdef tag (map (setSFItem f nms) items) } else sf
setSFItem _ _       x = x

setSFValue :: T.Text -> T.Text -> SFItem -> SFItem
setSFValue nm val = setSFItem (\_ -> DEVal (DEStr val)) (T.split (== '.') nm)

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

elemToDEGdef :: Element -> Maybe (T.Text, DEGdef)
elemToDEGdef (Element nm attrs ctnt _) = do
    when (qName nm /= "DEGdef") Nothing
    degid <- T.pack <$> findAttrByKey "id" attrs
    items <- L.foldl' f (Just []) (onlyElems ctnt)
    return (degid, DEGdef (findRequestTag attrs) (reverse items))
  where
    f (Just items) e = ((:items) <$> elemToDEdef e) <|>
                       ((\(x,y) -> map (setDEGValids x y) items) <$> elemToValids e) <|>
                       ((\(x,y) -> map (setDEGValue x y) items) <$> elemToValue e)
    f _ _            = Nothing

getCommonAttrsWDefault :: [Attr] -> (T.Text, Int, Maybe Int)
getCommonAttrsWDefault attrs = (name, minnum, maxnum)
  where
    name   = maybe "" T.pack (findAttrByKey "name" attrs)
    minnum = maybe 1 read (findAttrByKey "minnum" attrs)
    maxnum = maybe (Just 1) (\x -> let y = read x in if y == 0 then Nothing else Just y) (findAttrByKey "maxnum" attrs)

findRequestTag :: [Attr] -> Bool
findRequestTag = maybe False (== "1") . findAttrByKey "needsRequestTag"

lookupDeg :: M.Map T.Text DEGdef -> Element -> [DEGItem] -> Maybe [DEGItem]
lookupDeg degs (Element nm attrs _ _) items = do
  when (qName nm /= "DEG") Nothing
  type_ <- T.pack <$> findAttrByKey "type" attrs
  let (name, minnum, maxnum) = getCommonAttrsWDefault attrs
  degdef <- M.lookup type_ degs
  return $ DEG name minnum maxnum degdef : items

elemToSEGdef :: M.Map T.Text DEGdef -> Element -> Maybe (T.Text, SEGdef)
elemToSEGdef degs (Element nm attrs ctnt _) = do
  when (qName nm /= "SEGdef") Nothing
  id_ <- T.pack <$> findAttrByKey "id" attrs
  items <- L.foldl' f (Just []) (onlyElems ctnt)
  return (id_, SEGdef (findRequestTag attrs) (reverse items))
  where
    f (Just items) e = lookupDeg degs e items <|>
                       ((:items) <$> elemToDEdef e) <|>
                       ((\(x,y) -> map (setDEGValids x y) items) <$> elemToValids e) <|>
                       ((\(x,y) -> map (setDEGValue x y) items) <$> elemToValue e)
    f _ _            = Nothing

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
  return $ (id_, SFdef (findRequestTag attrs) items)

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

getXml :: String -> IO [Content]
getXml fname = do
  h <- openFile fname ReadMode
  xmlData <- BS.hGetContents h
  hClose h
  return $ parseXML xmlData

getChildrenByName :: String -> [Content] -> [Element]
getChildrenByName name ctnts = onlyElems ctnts >>= filterChildrenName (\x -> name == qName x) >>= elChildren

getDEGdefs :: [Content] -> M.Map T.Text DEGdef
getDEGdefs = M.fromList . catMaybes . map elemToDEGdef . getChildrenByName "DEGs"

getSEGdefs :: M.Map T.Text DEGdef -> [Content] -> M.Map T.Text SEGdef
getSEGdefs degs = M.fromList . catMaybes . map (elemToSEGdef degs) . getChildrenByName "SEGs"

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
