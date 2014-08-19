{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.HBCI.Types where

-- import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as M
import           Text.PrettyPrint


-- FIXME: Make things strict where appropriate

data DEValue = DEStr !T.Text
             | DEBinary !BS.ByteString
             deriving (Show, Eq)

type DEGValue = [DEValue]
type SEGValue = [DEGValue]
type MSGValue = [SEGValue]

data DEType = AN | Bin | Code | Ctr | Cur | DTAUS | Date | Dig | ID | JN | Num | Time | Wrt
           deriving (Eq, Show, Read)

data DE = DEdef { deName :: T.Text, deType :: DEType, deMinSize :: Int, deMaxSize :: Maybe Int,
                  deMinNum :: Int, deMaxNum :: Maybe Int, deValids :: Maybe [T.Text] }
        | DEval DEValue
        deriving (Eq, Show)

data DEG = DEG { degName :: T.Text, degMinNum :: Int, degMaxNum :: Maybe Int, degItems :: [DE] }
         deriving (Eq, Show)

data SEGItem = DEItem DE
             | DEGItem DEG
             deriving (Eq, Show)

data SEG = SEG { segName :: T.Text, needsRequestTag :: Bool, segMinNum :: Int, segMaxNum :: Maybe Int, segItems :: [SEGItem] }
         deriving (Eq, Show)

-- data SF = SF { sfMinNum :: Int, sfMaxNum :: Maybe Int, sfItems :: [SEG] }
--         deriving (Eq, Show)

data MSG = MSG { msgRequiresSignature :: Bool, msgRequiresEncryption :: Bool, msgItems :: [SEG] }
         deriving (Eq, Show)

data DEGEntry = DEentry !DEValue
              | DEGentry (M.Map T.Text DEValue)

type SEGEntry = M.Map T.Text DEGEntry

type MSGEntry = M.Map T.Text SEGEntry

data BankProperties = BankProperties { bankName :: !T.Text
                                     , bankCity :: !T.Text
                                     , bankBic :: !T.Text
                                     , bankHbciUrl :: !T.Text
                                     , bankPinTanUrl :: !T.Text
                                     , bankHbciVersion :: !T.Text
                                     , bankPinTanVersion :: !T.Text
                                     } deriving (Eq, Show)

class HbciPretty a where
  toDoc :: a -> Doc

  pprint :: a -> T.Text
  pprint = T.pack . show . toDoc

deLength :: DEValue -> Int
deLength (DEStr v)    = T.length v
deLength (DEBinary b) = let lengthBody   = BS.length b
                            lengthHeader = 2 + length (show lengthBody)
                        in lengthBody + lengthHeader

instance HbciPretty Int where
  toDoc = text . show

instance HbciPretty T.Text where
  toDoc = text . show

instance HbciPretty a => HbciPretty (Maybe a) where
  toDoc Nothing = text $ "Nothing"
  toDoc (Just a) = text "(Just " <> (toDoc a) <> char ')'

instance HbciPretty a => HbciPretty [a] where
  toDoc l = nest 0 (char '[' <> (vcat $ punctuate (char ',') $ map toDoc l) <> char ']')

instance HbciPretty DEValue where
  toDoc (DEStr x)    = text "(DEStr " <> text (show x) <> char ')'
  toDoc (DEBinary b) = text "(DEBinary " <> text (show b) <> char ')'

instance HbciPretty DE where
  toDoc (DEdef nm tp minSz maxSz minNum maxNum valids) = (hsep $ map text $ ["(DEdef", show nm, show tp, show minSz]) <+> toDoc maxSz <+> text (show minNum) <+> toDoc maxNum <+> toDoc valids <> char ')'
  toDoc (DEval val)                                    = text "(DEval " <> toDoc val <> char ')'

instance HbciPretty DEG where
  toDoc (DEG nm minNum maxNum des) = text "(DEG" <+> text (show nm) <+> int minNum <+> char '(' <> text (show maxNum) <> char ')'
                                     <+> toDoc des <> ")"

instance HbciPretty SEGItem where
  toDoc (DEItem de) = text "(DEItem " <> toDoc de <> char ')'
  toDoc (DEGItem deg) = text "(DEGItem " <> toDoc deg <> char ')'

instance HbciPretty SEG where
  toDoc (SEG nm tag minnum maxnum items) = text "(SEG" <+> text (show nm) <+> text (show tag) <+> toDoc minnum <+> toDoc maxnum <+> toDoc items <> char ')'

-- instance HbciPretty SF where
--   toDoc (SF minNum maxNum items) = text "(SF" <+> text (show minNum) <+> toDoc maxNum <+> toDoc items <> char ')'

instance HbciPretty MSG where
  toDoc (MSG sig enc items) = text "(MSG" <+> text (show sig) <+> text (show enc) <+> toDoc items <> char ')'
