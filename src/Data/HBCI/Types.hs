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
