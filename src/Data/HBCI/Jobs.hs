{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

module Data.HBCI.Jobs where

import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad (when)
import           Control.Monad.Trans.Either (left)
import           Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Text as T

import           Data.Time.Format (parseTime)
import           Data.Time.LocalTime (LocalTime)
import           System.Locale (defaultTimeLocale)

import           Data.HBCI.Types
import           Data.HBCI.Utils


class Job x where
  type JobResult x :: *

  getParams :: x -> HbciReader MSGEntry

  getResult :: x -> [(T.Text, DEValue)] -> Hbci (JobResult x)


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
    return $! M.fromList [("Saldo5", M.fromList [("KTV", DEGentry $ M.fromList [("number", DEStr accNum), ("KIK.country", DEStr "280"), ("KIK.blz", DEStr blz)])
                                                ,("allaccounts", DEentry (DEStr "N"))
                                                ])
                         ]

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
