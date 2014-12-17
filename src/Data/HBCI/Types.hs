{-# LANGUAGE OverloadedStrings, BangPatterns, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Data.HBCI.Types where

-- import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as M
import           Text.PrettyPrint

import           Control.Monad (liftM)
import           Control.Applicative (Applicative, (<$>))
import           Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)
import           Control.Monad.State (State, StateT, MonadState, get, put, runStateT, evalStateT, execStateT)
import           Control.Monad.Trans.Either (EitherT, hoistEither, left, right, runEitherT)
import           Control.Monad.Trans (MonadIO, lift, liftIO)
import           Control.Monad.Identity (Identity, runIdentity)

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


-- Idea: The HbciState should be run per user and per Bank. That way
-- we can keep things somewhat localized.
data BPD = BPD { bpdVersion    :: !T.Text
               , bpdMaxNumJobs :: !Int
               , bpdMsgSize    :: !Int
               , bpdOther      :: !(M.Map T.Text DEValue)
               } deriving (Eq, Show)

data UPD = UPD { updVersion :: !T.Text
               } deriving (Eq, Show)

data HbciUserInfo = HbciUserInfo { uiUserId :: T.Text
                                 , uiPIN    :: T.Text
                                 , uiBLZ    :: T.Text
                                 }

data HbciInfo = HbciInfo { hbciInfoBankProperties :: M.Map T.Text BankProperties
                         , hbciInfoMessages       :: M.Map T.Text MSG
                         , hbciInfoUserId         :: T.Text
                         , hbciInfoPin            :: T.Text
                         , hbciInfoBlz            :: T.Text
                         } deriving (Eq, Show)

data TanMode = MkTanMode { tanModeSecfunc :: !T.Text
                         , tanModeName    :: !T.Text
                         , tanModeId      :: !T.Text
                         } deriving (Eq, Show)

data HbciState = HbciState { hbciStateBPD      :: !(Maybe BPD)
                           , hbciStateUPD      :: !(Maybe UPD)
                           , hbciStateDialogID :: !T.Text -- FIXME: This should probably be 'Maybe T.Text'
                           , hbciStateMsgNum   :: !Int
                           , hbciStateSysId    :: !(Maybe T.Text)
                           , hbciStateTanModes :: !([TanMode])
                           } deriving (Eq, Show)


initialHbciState :: HbciState
initialHbciState = HbciState Nothing Nothing "0" 1 Nothing []


data HbciError =
  HbciErrorInputData T.Text
  | HbciErrorInternal T.Text
  | HbciErrorOther T.Text
  deriving Show


data HbciInfoInternal = HbciInfoInternal { infoInternalInfo :: HbciInfo, infoInternalState :: HbciState }

type Hbci' r m a = EitherT HbciError (ReaderT r m) a

-- newtype HbciReader a =
--   HbciReader { runHbciReader :: Hbci' HbciConfInternal Identity a
--              } deriving (Functor, Applicative, Monad, MonadReader HbciConfInternal)
--
-- newtype Hbci a =
--   Hbci { runHbci' :: Hbci' HbciConfig (State HbciState) a
--        } deriving (Functor, Applicative, Monad, MonadReader HbciConfig, MonadState HbciState)
--
-- newtype HbciIO a =
--   HbciIO { runHbciIO' :: Hbci' HbciConfig (StateT HbciState IO) a
--          } deriving (Functor, Applicative, Monad, MonadReader HbciConfig, MonadState HbciState, MonadIO)

type HbciReader a = Hbci' HbciInfoInternal Identity a

type Hbci a = Hbci' HbciInfo (State HbciState) a

type HbciIO a = Hbci' HbciInfo (StateT HbciState IO) a

runHbci :: HbciInfo -> HbciState -> Hbci a -> (Either HbciError a, HbciState)
runHbci info state action = runIdentity $! runStateT (runReaderT (runEitherT action) info) state

evalHbci :: HbciInfo -> HbciState -> Hbci b -> Either HbciError b
evalHbci info state action = fst $ runHbci info state action

askInfo :: MonadReader HbciInfoInternal m => m HbciInfo
askInfo = liftM infoInternalInfo $ ask

askState :: MonadReader HbciInfoInternal m => m HbciState
askState = liftM infoInternalState $ ask

liftReader :: (Monad m, MonadState HbciState m) => HbciReader a -> Hbci' HbciInfo m a
liftReader action = do
  info <- ask
  state <- get
  hoistEither $! runIdentity $! runReaderT (runEitherT action) (HbciInfoInternal info state)

liftHbci :: Hbci a -> HbciIO a
liftHbci action = do
  info <- ask
  state <- get
  let (res, state') = runHbci info state action
  put state'
  hoistEither res

runHbciIO :: HbciInfo -> HbciState -> HbciIO a -> IO (Either HbciError a, HbciState)
runHbciIO info state action = runStateT (runReaderT (runEitherT action) info) state

evalHbciIO :: HbciInfo -> HbciState -> HbciIO a -> IO (Either HbciError a)
evalHbciIO info state action = evalStateT (runReaderT (runEitherT action) info) state

-- type HBCI a = ReaderT HbciInfo (StateT HbciState (EitherT HbciError IO)) a
