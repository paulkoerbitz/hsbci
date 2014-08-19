{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Data.HBCI.HbciDef where

import           Control.Arrow (second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Traversable (traverse)
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad (foldM)
import           System.IO (openFile, hClose, IOMode(..))
import           Text.XML.Light
import           Control.Monad (when)

import           Data.HBCI.Types

findAttrByKey :: String -> [Attr] -> Maybe String
findAttrByKey k attr = attrVal <$> L.find (\a -> qName (attrKey a) == k) attr

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

setSEG :: (DE -> DE) -> [T.Text] -> SEG -> SEG
setSEG f nms seg = seg { segItems = setSEGItem f nms <$> segItems seg }

setValids :: ((DE -> DE) -> [T.Text] -> a -> a) -> T.Text -> [T.Text] -> a -> a
setValids f nm valids = f (\de -> de { deValids = Just valids }) (T.split (== '.') nm)

setValue :: ((DE -> DE) -> [T.Text] -> a -> a) -> T.Text -> T.Text -> a -> a
setValue f nm val = f (\_ -> DEval (DEStr val)) (T.split (== '.') nm)

setDEValids :: T.Text -> [T.Text] -> DE -> DE
setDEValids = setValids setDE

setDEValue :: T.Text -> T.Text -> DE -> DE
setDEValue = setValue setDE

setSEGItemValids :: T.Text -> [T.Text] -> SEGItem -> SEGItem
setSEGItemValids = setValids setSEGItem

setSEGItemValue :: T.Text -> T.Text -> SEGItem -> SEGItem
setSEGItemValue = setValue setSEGItem

setSEGValue :: T.Text -> T.Text -> SEG -> SEG
setSEGValue = setValue setSEG


{-
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
-}

err :: Maybe Line -> T.Text -> Either T.Text a
err (Just l) msg = Left $ T.pack (show l) <> ": " <> msg
err Nothing  msg = Left msg

merr :: Maybe Line -> T.Text -> Maybe a -> Either T.Text a
merr _    _   (Just x) = Right x
merr line msg Nothing  = err line msg

checkName :: String -> QName -> Maybe Line -> Either T.Text ()
checkName nm qnm line = do
  when (qName qnm /= nm) $ err line ("Element is not a " <> T.pack nm <> ": " <> T.pack (qName qnm))
  return ()

getRequiredAttr :: Maybe Line -> T.Text -> String -> [Attr] -> Either T.Text T.Text
getRequiredAttr line elemName attrName attrs =
  merr line ("Required attribute '" <> T.pack attrName <> "' missing in " <> elemName) $ T.pack <$> findAttrByKey attrName attrs

getReferencedItem :: T.Text -> Maybe Line -> M.Map T.Text a -> T.Text -> Either T.Text a
getReferencedItem name line map key =
  maybe (err line (name <> ": Referenced element '" <> key <> "' not found")) Right (M.lookup key map)

elemToDE :: Element -> Either T.Text DE
elemToDE (Element nm attrs _ line) = do
  checkName "DE" nm line
  tp <- merr line "Required attribute 'type' missing in DE" $ fst <$> (listToMaybe . reads =<< findAttrByKey "type" attrs)
  name <- getRequiredAttr line "DE" "name" attrs
  let minSz  = maybe 0  id $ (fst <$> (listToMaybe . reads =<< findAttrByKey "minsize" attrs ))
      maxSz  = fst <$> (listToMaybe . reads =<< findAttrByKey "maxsize" attrs)
      minNum = maybe 1 read (findAttrByKey "minnum" attrs)
      maxNum = findAttrByKey "maxnum" attrs >>= \x -> if x == "0" then Nothing else Just (read x)
  return $ DEdef name tp minSz maxSz minNum maxNum Nothing

elemToValids :: Element -> Either T.Text (T.Text, [T.Text])
elemToValids (Element nm attrs ctnt line) = do
  checkName "valids" nm line
  path <- getRequiredAttr line "valids" "path" attrs
  return (path, T.pack . strContent <$> onlyElems ctnt)

elemToValue :: Element -> Either T.Text (T.Text, T.Text)
elemToValue e@(Element nm attrs _ line) = do
  checkName "value" nm line
  path <- getRequiredAttr line "value" "path" attrs
  return (path, T.pack $ strContent e)

getDEGType :: Element -> Either T.Text T.Text
getDEGType (Element nm attrs _ line) = do
  checkName "DEG" nm line
  getRequiredAttr line "DEG" "type" attrs

-- FIXME: Some DEG references in xml set names and minnums ... thats
-- somewhat confusing - maybe I need to respect that when splicing
-- things in (?)
elemToDEG :: M.Map T.Text DEG -> Element -> Either T.Text (T.Text, DEG)
elemToDEG degs (Element nm attrs ctnt line) = do
  checkName "DEGdef" nm line
  id_ <- getRequiredAttr line "DEGdef" "id" attrs
  items <- foldM f [] (onlyElems ctnt)
  return (id_, DEG "" 0 Nothing (reverse items))
  where
    f items e | qName (elName e) == "DE"     = (:items) <$> elemToDE e
    f items e | qName (elName e) == "DEG"    = getDEGType e >>= getReferencedItem "DEGdef" line degs >>= return . (++items) . degItems
    f items e | qName (elName e) == "valids" = (\(x,y) -> map (setDEValids x y) items) <$> elemToValids e
    f items e | qName (elName e) == "value"  = (\(x,y) -> map (setDEValue x y) items) <$> elemToValue e
    f _     e                                = err (elLine e) ("Unexpected element while processing DEGdef: " <> T.pack (qName (elName e)))

findRequestTag :: [Attr] -> Bool
findRequestTag = maybe False (== "1") . findAttrByKey "needsRequestTag"

getCommonAttrsWDefault :: [Attr] -> (T.Text, Int, Maybe Int)
getCommonAttrsWDefault attrs = (name, minnum, maxnum)
  where
    name   = maybe "" T.pack $ findAttrByKey "name" attrs <|> findAttrByKey "type" attrs
    minnum = maybe 1 read (findAttrByKey "minnum" attrs)
    maxnum = maybe (Just 1) (\x -> let y = read x in if y == 0 then Nothing else Just y) (findAttrByKey "maxnum" attrs)

elemToSEG :: M.Map T.Text DEG -> Element -> Either T.Text (T.Text, SEG)
elemToSEG degs (Element nm attrs ctnt line) = do
  checkName "SEGdef" nm line
  id_ <- getRequiredAttr line "SEGdef" "id" attrs
  items <- foldM f [] (onlyElems ctnt)
  return (id_, SEG "" (findRequestTag attrs) 0 Nothing (reverse items))
  where
    f items e | qName (elName e) == "DE"     = (:items) . DEItem <$> elemToDE e
    f items e | qName (elName e) == "DEG"    = let (refNm, minnum, maxnum) = getCommonAttrsWDefault (elAttribs e)
                                               in do tp <- getRequiredAttr line (T.pack (qName (elName e))) "type" (elAttribs e)
                                                     (DEG _ _ _ des) <- getReferencedItem "DEG" (elLine e) degs tp
                                                     return $ DEGItem (DEG refNm minnum maxnum des) : items
    f items e | qName (elName e) == "valids" = (\(x,y) -> map (setSEGItemValids x y) items) <$> elemToValids e
    f items e | qName (elName e) == "value"  = (\(x,y) -> map (setSEGItemValue x y) items) <$> elemToValue e
    f _     e                                = err (elLine e) ("Unexpected element while processing SEGdef: " <> T.pack (qName (elName e)))

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
-- too simple in some cases. There are no SFs, these are just lists of
-- SEGs and the minnums and maxnums live directly within the SEGs.
-- This might be too simplistic in some cases, I'll fix that if it
-- turns out to be the case.
elemToSFItem :: M.Map T.Text SEG -> M.Map T.Text [SEG] -> Element -> Either T.Text [SEG]
elemToSFItem segs sfs (Element nm attrs _ line) = do
  type_ <- getRequiredAttr line (T.pack (qName nm)) "type" attrs
  let (name, minnum, maxnum) = getCommonAttrsWDefault attrs
  if (qName nm == "SF")
  then getReferencedItem "SF" line sfs type_ >>= return . map (updateMinMax minnum maxnum)
  else if (qName nm == "SEG")
       then getReferencedItem "SEG" line segs type_ >>= \(SEG _ tag _ _ items) -> return [SEG name tag minnum maxnum items]
       else err line ("Not a SF or SEG: " <> T.pack (qName nm))
  where
    updateMinMax outerMin outerMax seg@(SEG _ _ innerMin innerMax _) =
      let newMin = min innerMin outerMin
          newMax = max <$> innerMax <*> outerMax
      in seg { segMinNum = newMin, segMaxNum = newMax }

elemToSF :: M.Map T.Text SEG -> M.Map T.Text [SEG] -> Element -> Either T.Text (T.Text, [SEG])
elemToSF segs sfs (Element nm attrs ctnt line) =  do
  checkName "SFdef" nm line
  id_ <- getRequiredAttr line "SFdef" "id" attrs
  items <- concat <$> traverse (elemToSFItem segs sfs) (onlyElems ctnt)
  return $ (id_, items)

elemToMSG :: M.Map T.Text SEG -> M.Map T.Text [SEG] -> Element -> Either T.Text (T.Text, MSG)
elemToMSG segs sfs (Element nm attrs ctnt line) = do
  checkName "MSGdef" nm line
  id_ <- getRequiredAttr line "MSGdef" "id" attrs
  let reqSig   = maybe True (/="1") $ findAttrByKey "dontsign" attrs
      reqCrypt = maybe True (/="1") $ findAttrByKey "dontcrypt" attrs
  items <- foldM f [] (onlyElems ctnt)
  return $ (id_, MSG reqSig reqCrypt items)
  where
    f items e | qName (elName e) == "SF" || qName (elName e) == "SEG" = (items ++) <$> elemToSFItem segs sfs e
    f items e | qName (elName e) == "value"                           = (\(x,y) -> map (setSEGValue x y) items) <$> elemToValue e
    f _     e                                                         = err (elLine e) ("Unexpected element while processing MSGdef: " <> T.pack (qName (elName e)))

getXml :: String -> IO [Content]
getXml fname = do
  h <- openFile fname ReadMode
  xmlData <- BS.hGetContents h
  hClose h
  return $ parseXML xmlData

getChildrenByName :: String -> [Content] -> [Element]
getChildrenByName name ctnts = onlyElems ctnts >>= filterChildrenName (\x -> name == qName x) >>= elChildren

getDEGs :: [Content] -> Either T.Text (M.Map T.Text DEG)
getDEGs = foldM f M.empty . getChildrenByName "DEGs"
  where
    f degs e = elemToDEG degs e >>= \(id_, deg) -> return (M.insert id_ deg degs)

getSEGs :: M.Map T.Text DEG -> [Content] -> Either T.Text (M.Map T.Text SEG)
getSEGs degs ctnt = M.fromList <$> mapM (elemToSEG degs) (getChildrenByName "SEGs" ctnt)

getSFs :: M.Map T.Text SEG -> [Content] -> Either T.Text (M.Map T.Text [SEG])
getSFs segs ctnt = foldM f M.empty $ getChildrenByName "SFs" ctnt
  where
    f sfs e = elemToSF segs sfs e >>= \(id_, sf) -> return (M.insert id_ sf sfs)

getMSGs :: M.Map T.Text SEG -> M.Map T.Text [SEG] -> [Content] -> Either T.Text (M.Map T.Text MSG)
getMSGs segs sfs ctnt = M.fromList <$> mapM (elemToMSG segs sfs) (getChildrenByName "MSGs" ctnt)

getMSGfromXML :: [Content] -> Either T.Text (M.Map T.Text MSG)
getMSGfromXML xml = do
  degs <- getDEGs xml
  segs <- getSEGs degs xml
  sfs  <- getSFs segs xml
  getMSGs segs sfs xml

parseBankPropsLine :: BS.ByteString -> Either T.Text (T.Text, BankProperties)
parseBankPropsLine line = do
  when (T.length blz /= 8) $ Left ("BLZ '" <> blz <> "' has wrong format")
  when (length props /= 9) $ Left ("Properties have the wrong format")
  let (name:city:bic:_:hbciUrl:pinTanUrl:hbciVersion:pinTanVersion:_) = props
  return (blz, BankProperties name city bic hbciUrl pinTanUrl hbciVersion pinTanVersion)
  where
    (blz, props) = second (T.split (== '|') . T.drop 1) $ T.break (== '=') $ E.decodeUtf8 line

getBankPropsFromFile :: FilePath -> IO (Either T.Text (M.Map T.Text BankProperties))
getBankPropsFromFile fname = do
  ctnt <- BS.readFile fname
  return (M.fromList <$>  mapM parseBankPropsLine (filter (\s -> BS.length s > 0) $ C8.split '\n' ctnt))
