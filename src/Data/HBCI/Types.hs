{-# LANGUAGE BangPatterns #-}
module Data.HBCI.Types where

-- import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS

-- FIXME: Make things strict where appropriate

data DEValue = DEStr !T.Text
             | DEBinary !BS.ByteString
             deriving (Show, Eq)

type DEGValue = [DEValue]
type SEGValue = [DEGValue]
type MSGValue = [SEGValue]

data DEType = AN | Bin | Code | Ctr | Cur | DTAUS | Date | Dig | ID | JN | Num | Time | Wrt
           deriving (Eq, Show, Read)

data DEGdef = DEGdef { degNeedsRequestTag :: Bool, degItems :: [DEGItem] }
            deriving (Eq, Show)

data DEGItem = DE  { deName :: T.Text, deType :: DEType, deMinSize :: Int, deMaxSize :: Maybe Int,
                     deMinNum :: Int, deMaxNum :: Maybe Int, deValids :: Maybe [T.Text] }
             | DEG { degName :: T.Text, degMinNum :: Int, degMaxNum :: Maybe Int, degDef :: DEGdef }
             | DEVal DEValue
             deriving (Eq, Show)

data SEGdef = SEGdef { needsRequestTag :: Bool, segItems :: [DEGItem] }
            deriving (Eq, Show)

data SFItem = SEG { sfiName :: T.Text, sfiMinNum :: Int, sfiMaxNum :: Maybe Int, sfiSegDef :: SEGdef }
            | SF  { sfiMinNum :: Int, sfiMaxNum :: Maybe Int, sfiSfDef :: SFdef }
            deriving (Eq, Show)

data SFdef = SFdef { sfdefNeedsRequestTag :: Bool, sfItems :: [SFItem] }
           deriving (Eq, Show)

data MSGdef = MSGdef { msgRequiresSignature :: Bool, msgRequiresEncryption :: Bool, msgItems :: [SFItem] }
            deriving (Eq, Show)
