{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.HBCI.Types where

-- import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS
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

data SEG = SEG { segName :: T.Text, needsRequestTag :: Bool, segItems :: [SEGItem] }
         deriving (Eq, Show)

data SF = SF { sfMinNum :: Int, sfMaxNum :: Maybe Int, sfItems :: [SEG] }
        deriving (Eq, Show)

data MSG = MSG { msgRequiresSignature :: Bool, msgRequiresEncryption :: Bool, msgItems :: [SF] }
         deriving (Eq, Show)

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

instance HbciPretty DEValue where
  toDoc (DEStr x)    = text $ T.unpack x
  toDoc (DEBinary b) = text $ "@" ++ (show (deLength (DEBinary b))) ++ "@" ++ show b

instance HbciPretty DE where
  toDoc (DEdef nm tp minSz maxSz minNum maxNum valids) = (hsep $ map text $ ["{DEdef", T.unpack nm, show tp, show minSz, show maxSz, show minNum, show maxNum, show valids]) <> text "}"
  toDoc (DEval val)                                    = (hsep $ map text $ ["{DEval", show val]) <> "}"

instance HbciPretty DEG where
  toDoc (DEG nm minNum maxNum des) = text "{DEG" <+> text (show nm) <+> int minNum <+> char '(' <> text (show maxNum) <> char ')' <+> nest 10 (vcat $ map toDoc des) <> "}"
