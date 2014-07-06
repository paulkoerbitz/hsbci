{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.HbciDef where

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Maybe (catMaybes, listToMaybe)
import           Control.Applicative ((<$>))
import           System.IO (openFile, hClose, IOMode(..))
import           Text.XML.Light
import           Control.Monad (when)

-- FIXME: Make things strict where appropriate
data DEType = AN | Bin | Code | Ctr | Cur | DTAUS | Date | Dig | ID | JN | Num | Time | Wrt
           deriving (Eq, Show, Read)

data DEGdef = DEGdef { degId :: T.Text, degNeedsRequestTag :: Bool, degItems :: [DEGItem], degValues :: [(T.Text,T.Text)] }
            deriving (Eq, Show)

data DEGItem = DE  { deName :: T.Text, deType :: DEType, deMinSize :: Int, deMaxSize :: Maybe Int,
                     deMinNum :: Int, deMaxNum :: Maybe Int, deValids :: Maybe [T.Text] }
             | DEG { degName :: T.Text, degMinNum :: Int, degMaxNum :: Maybe Int, degDef :: DEGdef }
             deriving (Eq, Show)

data SEGdef = SEGdef { segId :: T.Text, needsRequestTag :: Bool, segItems :: [DEGItem], segValues :: [(T.Text,T.Text)] }
            deriving (Eq, Show)

data SFItem = SEG { sfiName :: T.Text, sfiMinNum :: Int, sfiMaxNum :: Maybe Int, sfiSegDef :: SEGdef }
            | SF  { sfiName :: T.Text, sfiMinNum :: Int, sfiMaxNum :: Maybe Int, sfiSfDef :: SFdef }
            deriving (Eq, Show)

data SFdef = SFdef { sfdefId :: T.Text, sfdefNeedsRequestTag :: Bool, sfItems :: [SFItem] }
           deriving (Eq, Show)

data MSGdef = MSGdef { msgId :: T.Text, msgRequiresSignature :: Bool, msgRequiresEncryption :: Bool, msgItems :: [SFItem],
                       msgValues :: [(T.Text, T.Text)] }
            deriving (Eq, Show)

findAttrByKey :: String -> [Attr] -> Maybe String
findAttrByKey k attr = attrVal <$> L.find (\a -> qName (attrKey a) == k) attr

elemToDEdef :: Element -> Maybe DEGItem
elemToDEdef (Element nm attrs _ _) = do
    when (qName nm /= "DE") Nothing
    name <- T.pack <$> findAttrByKey "name" attrs
    tp <- fst <$> (listToMaybe . reads . (\x -> trace ("x=" ++ x) x)  =<< findAttrByKey "type" attrs)
    let minSz  = maybe 0  id $ (fst <$> (listToMaybe . reads =<< findAttrByKey "minsize" attrs ))
        maxSz  = fst <$> (listToMaybe . reads =<< findAttrByKey "maxsize" attrs)
        minNum = maybe 1 read (findAttrByKey "minnum" attrs)
        maxNum = findAttrByKey "maxnum" attrs >>= \x -> if x == "0" then Nothing else Just (read x)
    return $ DE name tp minSz maxSz minNum maxNum Nothing

updateValids :: [Element] -> DEGItem -> DEGItem
updateValids es item = if (isDE item) then updateValid (findValids es) item else item
  where
    isDE (DE _ _ _ _ _ _ _) = True  -- FIXME: This could probably be done more nicely
    isDE _                  = False -- with a prism

    getValid (Element nm attrs ctnt _) = do
          when (qName nm /= "valids") Nothing
          path <- findAttrByKey "path" attrs
          return (T.pack path, T.pack . strContent <$> onlyElems ctnt)

    findValids = catMaybes . map getValid

    updateValid vals de = case lookup (deName de) vals of
      Nothing -> de
      Just items -> de { deValids = Just items }

elemToValue :: Element -> Maybe (T.Text, T.Text)
elemToValue e@(Element nm attrs _ _) = do
  when (qName nm /= "value") Nothing
  path <- findAttrByKey "path" attrs
  return (T.pack path, T.pack $ strContent e)

elemToDEGdef :: Element -> Maybe DEGdef
elemToDEGdef (Element nm attrs ctnt _) = do
    when (qName nm /= "DEGdef") Nothing
    degid <- T.pack <$> findAttrByKey "id" attrs
    let elems  = onlyElems ctnt
        degdes = catMaybes $ map elemToDEdef elems
        values = catMaybes $ map elemToValue elems
    return $ DEGdef degid (findRequestTag attrs) (map (updateValids elems) degdes) values

getCommonAttrsWDefault :: [Attr] -> (T.Text, Int, Maybe Int)
getCommonAttrsWDefault attrs = (name, minnum, maxnum)
  where
    name   = maybe "" T.pack (findAttrByKey "name" attrs)
    minnum = maybe 1 read (findAttrByKey "minnum" attrs)
    maxnum = maybe (Just 1) (\x -> let y = read x in if y == 0 then Nothing else Just y) (findAttrByKey "maxnum" attrs)

findRequestTag :: [Attr] -> Bool
findRequestTag = maybe False (== "1") . findAttrByKey "needsRequestTag"

elemToSEGdef :: M.Map T.Text DEGdef -> Element -> Maybe SEGdef
elemToSEGdef degs (Element nm attrs ctnt _) = do
  when (qName nm /= "SEGdef") Nothing
  id_ <- T.pack <$> findAttrByKey "id" attrs
  go [] (onlyElems ctnt) $ SEGdef id_ (findRequestTag attrs) [] []
  where
    go valids []                               seg                        =
      Just $ seg { segItems = reverse $ map (updateValids valids) $ segItems seg }
    go valids ((Element nm attrs ctnt _):es)   seg | qName nm == "DEG"    = do
      type_ <- T.pack <$> findAttrByKey "type" attrs
      let (name, minnum, maxnum) = getCommonAttrsWDefault attrs
      degdef <- M.lookup type_ degs
      go valids es $ seg { segItems = DEG name minnum maxnum degdef : segItems seg }
    go valids (e@(Element nm attrs ctnt _):es) seg | qName nm == "DE"     = do
      de <- elemToDEdef e
      go valids es $ seg { segItems = de : segItems seg }
    go valids (e@(Element nm attrs ctnt _):es) seg | qName nm == "valids" = go (e:valids) es seg
    go valids (e@(Element nm _ _ _):es) seg | qName nm == "value"  = do
      value <- elemToValue e
      go valids es $ seg { segValues = value : segValues seg }
    go _      (_:_)                         _                        = Nothing

elemToSFItem :: M.Map T.Text SEGdef -> M.Map T.Text SFdef -> Element -> Maybe SFItem
elemToSFItem segs sfs (Element nm attrs _ _) = do
  type_ <- T.pack <$> findAttrByKey "type" attrs
  let (name, minnum, maxnum) = getCommonAttrsWDefault attrs
  if (qName nm == "SF")
  then M.lookup type_ sfs >>= \sf -> return $ SF name minnum maxnum sf
  else if (qName nm == "SEG")
       then M.lookup type_ segs >>= \seg -> return $ SEG name minnum maxnum seg
       else Nothing

elemToSFdef :: M.Map T.Text SEGdef -> M.Map T.Text SFdef -> Element -> Maybe SFdef
elemToSFdef segs sfs (Element nm attrs ctnt _) = do
  when (qName nm /= "SFdef") Nothing
  id_ <- T.pack <$> findAttrByKey "id" attrs
  items <- sequence . map (elemToSFItem segs sfs) $ onlyElems ctnt
  return $ SFdef id_ (findRequestTag attrs) items

elemToMSGdef :: M.Map T.Text SEGdef -> M.Map T.Text SFdef -> Element -> Maybe MSGdef
elemToMSGdef segs sfs (Element nm attrs ctnt _) = do
  when (qName nm /= "MSGdef") Nothing
  id_ <- T.pack <$> findAttrByKey "id" attrs
  let reqSig   = maybe True (/="1") $ findAttrByKey "dontsign" attrs
      reqCrypt = maybe True (/="1") $ findAttrByKey "dontcrypt" attrs
      elems    = onlyElems ctnt
      values   = catMaybes $ map elemToValue elems
  items <- sequence . map (elemToSFItem segs sfs) $ elems
  return $ MSGdef id_ reqSig reqCrypt items values


getXml :: String -> IO [Content]
getXml fname = do
  h <- openFile fname ReadMode
  xmlData <- BS.hGetContents h
  hClose h
  return $ parseXML xmlData

getChildrenByName :: String -> [Content] -> [Element]
getChildrenByName name ctnts = onlyElems ctnts >>= filterChildrenName (\x -> name == qName x) >>= elChildren

getDEGdefs :: [Content] -> M.Map T.Text DEGdef
getDEGdefs = M.fromList . foldr f [] . getChildrenByName "DEGs"
  where
    f e acc = maybe acc (\x -> (degId x, x):acc) $  elemToDEGdef e

getSEGdefs :: M.Map T.Text DEGdef -> [Content] -> M.Map T.Text SEGdef
getSEGdefs degs = M.fromList . foldr f [] . getChildrenByName "SEGs"
    where
      f e acc = maybe acc (\x -> (segId x, x):acc) $ elemToSEGdef degs e

getSFdefs :: M.Map T.Text SEGdef -> [Content] -> M.Map T.Text SFdef
getSFdefs segs = foldr f M.empty . getChildrenByName "SFs"
    where
      f e sfs = maybe sfs (\x -> M.insert (sfdefId x) x sfs) $ elemToSFdef segs sfs e

getMSGdefs :: M.Map T.Text SEGdef -> M.Map T.Text SFdef -> [Content] -> M.Map T.Text MSGdef
getMSGdefs segs sfs = M.fromList . foldr f [] . getChildrenByName "MSGs"
    where
      f e acc = maybe acc (\x -> (msgId x, x):acc) $ elemToMSGdef segs sfs e
