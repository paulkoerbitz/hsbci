{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

module Data.HBCI.Jobs where

import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad (when, liftM)
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

  getResult :: x -> [(T.Text, DEValue)] -> Hbci (JobResult x)


------------------------------------------------------------------------
-- Get Balance
------------------------------------------------------------------------
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
    -- FIXME: Probably need to get the version of the supported saldo method from the BPD
    blz <- hbciInfoBlz <$> askInfo
    return $! M.fromList [("Saldo5", M.fromList [("KTV", ktv2 accNum "" blz) ,("allaccounts", DEentry (DEStr "N"))])]

  -- FIXME: The format that the parser spits out should probably  be adjusted so that we
  -- can handle multiple Saldo results
  getResult (GetBalance acntNum) vals = do
    when (((acntNum ==) <$> (lookup "SaldoRes5.KTV.number" vals >>= deToTxt)) /= Just True) $!
      left $! HbciErrorInternal "Bank returned balance for wrong bank account: Currently only one account can be handled!"
    bookedBal' <- fromMaybe (HbciErrorInternal "Saldo5: Didn't find expected field 'booked.BTG.value'") bookedBal
    return $! GetBalanceResult bookedBal' unbookedBal overdraft available used bookingTime
    where
      currency    = toCurrency =<< deToTxt  =<< lookup "SaldoRes5.curr" vals
      toAmount mx = Amount <$> mx <*> currency
      bookedBal   = toAmount $ btgToInt =<< lookup "SaldoRes5.booked.BTG.value" vals
      unbookedBal = toAmount $ btgToInt =<< lookup "SaldoRes5.pending.BTG.value" vals
      overdraft   = toAmount $ btgToInt =<< lookup "SaldoRes5.kredit.value" vals
      available   = toAmount $ btgToInt =<< lookup "SaldoRes5.available.value" vals
      used        = toAmount $ btgToInt =<< lookup "SaldoRes5.used.value" vals
      bookingTime = let mDateString     = deToTxt =<< lookup "SaldoRes5.booked.date" vals
                        mTimeString     = (deToTxt =<< lookup "SaldoRes5.booked.time" vals) <|> Just "000000"
                        mDateTimeString = (<>) <$> mDateString <*> mTimeString
                    in parseTime defaultTimeLocale "%0Y%m%d%H%M%S" . T.unpack =<< mDateTimeString

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
  getResult _ vals = do
    -- when (((acntNum ==) <$> (lookup "KUmsRes5.KTV.number" vals >>= deToTxt)) /= Just True) $!
    --   left $! HbciErrorInternal "Bank returned balance for wrong bank account: Currently only one account can be handled!"
    booked' <- return $! maybe [] id booked
    unbooked' <- return $! maybe [] id unbooked
    return $! GetStatementListResult booked' unbooked'
    where
      booked = parseMt94x =<< deToBS =<< lookup "KUmsZeitRes5.booked" vals
      unbooked = parseMt94x =<< deToBS =<< lookup "KUmsZeitRes5.unbooked" vals


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


ktv2 :: T.Text -> T.Text -> T.Text -> DEGEntry
ktv2 actNum subNum blz = DEGentry $! M.fromList $!
                         [("number", DEStr actNum), ("KIK.country", DEStr "280"), ("KIK.blz", DEStr blz)] ++
                         if T.null subNum then [] else [("subnumber", DEStr subNum)]

formatTime' = formatTime defaultTimeLocale

fst3 (x,_,_) = x
