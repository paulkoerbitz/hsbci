{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

module Data.HBCI.Jobs where

import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad (when, liftM)
import           Control.Monad.State (get)
import           Control.Monad.Trans.Either (left)
import           Data.Monoid ((<>))
import           Data.Char (ord)
import           Data.Word (Word8)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeLatin1)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Control.Monad.ST (runST, ST)
import           Control.Monad.Trans (lift, MonadTrans)
import           Data.STRef (newSTRef, readSTRef, modifySTRef, STRef)
import           Data.List (sort)
import           Data.Maybe (catMaybes, listToMaybe)

import           Data.Time.Format (parseTime, formatTime)
import           Data.Time.LocalTime (LocalTime)
import           Data.Time.Calendar (Day, toGregorian, diffDays)
import           System.Locale (defaultTimeLocale)

import           Data.HBCI.Types
import           Data.HBCI.Utils
import           Data.HBCI.Mt94x


class Job x where
  type JobResult x :: *

  getParams :: x -> HbciReader MSGEntry

  getResult :: x -> [(T.Text, Int)] -> MsgData -> Hbci (JobResult x)


------------------------------------------------------------------------
-- Get Balance
------------------------------------------------------------------------
safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum []     = Nothing
safeMaximum (x:xs) = (safeMaximum xs >>= return . max x) <|> Just x

getMaxJobVersion :: T.Text -> HbciReader Int
getMaxJobVersion name = do
  bpd <- hbciStateBPD <$> askState
  fromMaybe (HbciErrorInternal $! "Job " <> name <> ": Could not find version number in BPD") $!
    safeMaximum . fmap hbciJobParamsVersion =<< M.lookup name . bpdJobParams =<< bpd

findResultSegs :: Job x => x -> [(T.Text, Int)] -> MsgData -> HbciReader [(T.Text, [(T.Text, DEValue)])]
findResultSegs = undefined

data GetBalance =
  GetBalance { gbAccountNumber :: T.Text
             }

data GetBalanceResult =
  GetBalanceResult { gbrBookedBalance   :: Amount
                   , gbrUnbookedBalance :: Maybe Amount
                   , gbrOverdraftLimit  :: Maybe Amount
                   , gbrAvailableAmount :: Maybe Amount
                   , gbrUsedAmount      :: Maybe Amount
                   , gbrBookingTime     :: Maybe LocalTime
                   } deriving Show

instance Job GetBalance where
  type JobResult GetBalance = GetBalanceResult

  getParams (GetBalance accNum) = do
    jobName <- getMaxJobVersion "Saldo" >>= \x -> return $ "Saldo" <> T.pack (show x)
    blz <- hbciInfoBlz <$> askInfo
    return $! M.fromList [(jobName, M.fromList [("KTV", ktv2 accNum "" blz) ,("allaccounts", DEentry (DEStr "N"))])]

  getResult job@(GetBalance acntNum) requestSegNums vals' = do
    resultSegs <- liftReader $! findResultSegs job requestSegNums vals'
    resultXmlName <- liftReader $! getMaxJobVersion "Saldo" >>= \x -> return ("SaldoRes" <> T.pack (show x))
    processSegs resultXmlName resultSegs
    where
      processSegs expectedNm [(nm, vals)] | nm == expectedNm = do
        when (((acntNum ==) <$> (lookup "KTV.number" vals >>= deToTxt)) /= Just True) $!
          left $! HbciErrorInternal "Bank returned balance for wrong bank account: Currently only one account can be handled!"
        let currency    = toCurrency =<< deToTxt  =<< lookup "curr" vals
            toAmount mx = Amount <$> mx <*> currency
            bookedBal   = toAmount $ btgToInt =<< lookup "booked.BTG.value" vals
            unbookedBal = toAmount $ btgToInt =<< lookup "pending.BTG.value" vals
            overdraft   = toAmount $ btgToInt =<< lookup "kredit.value" vals
            available   = toAmount $ btgToInt =<< lookup "available.value" vals
            used        = toAmount $ btgToInt =<< lookup "used.value" vals
            bookingTime = let mDateString     = deToTxt =<< lookup "booked.date" vals
                              mTimeString     = (deToTxt =<< lookup "booked.time" vals) <|> Just "000000"
                              mDateTimeString = (<>) <$> mDateString <*> mTimeString
                          in parseTime defaultTimeLocale "%0Y%m%d%H%M%S" . T.unpack =<< mDateTimeString
        bookedBal' <- fromMaybe (HbciErrorInternal "Saldo: Didn't find expected field 'booked.BTG.value'") bookedBal
        return $! GetBalanceResult bookedBal' unbookedBal overdraft available used bookingTime

      processSegs _ _  = left $! HbciErrorInternal $! "Job Saldo: Found unknown result segments while processing response"


{-
------------------------------------------------------------------------
-- Get Statement List
------------------------------------------------------------------------
data GetStatementList =
  GetStatementList { gslAccountNumber    :: T.Text
                   , gslAccountSubNumber :: T.Text
                   , gslStartDate        :: Day
                   , gslEndDate          :: Day
                   , gslMaxEntries       :: Int
                   } deriving Show

data GetStatementListResult =
  GetStatementListResult { gslrBookedEntries :: [StatementEntry]
                         , gslrUnbookedEntries :: [StatementEntry]
                         } deriving Show


deToBS :: DEValue -> Maybe BS.ByteString
deToBS (DEBinary bs) = Just bs
deToBS _             = Nothing

instance Job GetStatementList where
  type JobResult GetStatementList = GetStatementListResult

  getParams (GetStatementList actNum subNum start end maxEntries) = do
    blz <- hbciInfoBlz <$> askInfo
    return $! M.fromList [("KUmsZeit5", M.fromList [("KTV", ktv2 actNum subNum blz)
                                                   ,("allaccounts", DEentry (DEStr "N"))
                                                   ,("startdate", DEentry $! DEStr $! T.pack $! formatTime' "%0Y%m%d" start)
                                                   ,("enddate", DEentry $! DEStr $! T.pack $! formatTime' "%0Y%m%d" end)
                                                   ,("maxentries", DEentry (DEStr $! if maxEntries > 0 then T.pack $! show maxEntries else ""))])]

  -- FIXME: The format that the parser spits out should probably  be adjusted so that we
  -- can handle multiple Saldo results -- I need to check which segment this one is referring to (on
  -- a higher level
  getResult _ _ _vals = do
    -- when (((acntNum ==) <$> (lookup "KUmsRes5.KTV.number" vals >>= deToTxt)) /= Just True) $!
    --   left $! HbciErrorInternal "Bank returned balance for wrong bank account: Currently only one account can be handled!"
    booked' <- return $! maybe [] id booked
    unbooked' <- return $! maybe [] id unbooked
    return $! GetStatementListResult booked' unbooked'
    where
      booked = undefined -- parseMt94x =<< deToBS =<< lookup "KUmsZeitRes5.booked" vals
      unbooked = undefined -- parseMt94x =<< deToBS =<< lookup "KUmsZeitRes5.unbooked" vals


------------------------------------------------------------------------
-- Start Transaction
------------------------------------------------------------------------
data StartTransaction =
  StartTransaction { stSrcIban         :: T.Text
                   , stSrcBic          :: T.Text
                   , stTgtIban         :: T.Text
                   , stTgtBic          :: T.Text
                   , stBeneficiaryName :: T.Text
                   , stAmount          :: Amount
                   , stPaymentPurpose  :: [T.Text]
                   }

-- FIXME
data CompleteTransactionInfo = CompleteTransactionInfo
                             deriving Show

data StartTransactionResult =
  StartTransactionResult { strCompleteTransactionInfo :: CompleteTransactionInfo
                         } deriving Show

instance Job StartTransaction where
  type JobResult StartTransaction = StartTransactionResult

  getParams _st = undefined

  getResult _st _vals = undefined
-}

ktv2 :: T.Text -> T.Text -> T.Text -> DEGEntry
ktv2 actNum subNum blz = DEGentry $! M.fromList $!
                         [("number", DEStr actNum), ("KIK.country", DEStr "280"), ("KIK.blz", DEStr blz)] ++
                         if T.null subNum then [] else [("subnumber", DEStr subNum)]

formatTime' :: String -> Day -> String
formatTime' = formatTime defaultTimeLocale

fst3 :: (t, t1, t2) -> t
fst3 (x,_,_) = x
