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
data StatementEntry =
  StatementEntry { seValutaDate     :: Day
                 , seBookingDate    :: Day
                 , seRecipient      :: T.Text
                 , sePaymentPurpose :: [T.Text]
                 , seAmount         :: Amount
                 } deriving Show

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

splitBs :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
splitBs _ txt   | BS.null txt = []
splitBs sep txt               =  let (chunk, rest) = BS.breakSubstring sep txt
                                 in chunk : splitBs sep rest

data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMT m f = MaybeT $ do
  inner <- runMaybeT m
  case inner of
    Nothing -> return Nothing
    Just x -> runMaybeT (f x)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT = MaybeT . return . Just

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  fail = failMT

instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

newSTRef' :: (MonadTrans t) => a -> t (ST s) (STRef s a)
newSTRef' = lift . newSTRef

readSTRef' :: (MonadTrans t) => STRef s a -> t (ST s) a
readSTRef' = lift . readSTRef

modifySTRef' :: (MonadTrans t) => STRef s a -> (a -> a) -> t (ST s) ()
modifySTRef' ref = lift . modifySTRef ref

hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe (Just a) = return a
hoistMaybe Nothing  = MaybeT $! return Nothing

c2w :: Char -> Word8
c2w = fromIntegral . ord

parseMt94x :: BS.ByteString -> Maybe [StatementEntry]
parseMt94x bs = process chunks
  where
    chunks = filter (\x -> ":61:" `BS.isPrefixOf` x || ":86:" `BS.isPrefixOf` x) $! splitBs "\r\n" bs

    process (x:y:zs) = if ":61:" `BS.isPrefixOf` x && ":86:" `BS.isPrefixOf` y
                       then do (valuta, booking, amt) <- parse61 x
                               purpose <- parse86 y
                               rest <- process zs
                               return $! (StatementEntry valuta booking "" purpose amt):rest
                       else process (y:zs)
    process (x:xs)   = if ":61:" `BS.isPrefixOf` x
                       then do (valuta, booking, amt) <- parse61 x
                               rest <- process xs
                               return $! (StatementEntry valuta booking "" [] amt):rest
                       else process xs
    process []       = return []

    isDigit c = c >= c2w '0' && c <= c2w '9'

    readAmt :: BS.ByteString -> Maybe Int
    readAmt amtStr = do
      commaPos <- BS.elemIndex (c2w ',') amtStr
      let eurStr = BS.take commaPos bs
          centStr = BS.takeWhile isDigit $! BS.drop commaPos bs
      (euros, _) <- C8.readInt eurStr
      if BS.null centStr
        then return $! 100*euros
        else do (cents, _) <- C8.readInt centStr
                return $! 100*euros + cents

    findBooking :: Day -> BS.ByteString -> Maybe Day
    findBooking valuta bookingStr =
      let valutaYear        = (\(x,_,_) -> x) $! toGregorian $! valuta
          candidateYears    = [pred, id, succ] <*> [valutaYear] :: [Integer]
          bookingCandidates = catMaybes $! map (\yr -> parseTime' "%Y%m%d" (show yr ++ C8.unpack (BS.take 4 bookingStr))) candidateYears
          tuples            = map (\dt -> (abs (dt `diffDays` valuta), dt)) bookingCandidates
      in snd <$> listToMaybe (sort tuples)

    -- This is woefully incomplete and a lot more interesting stuff could be pulled
    -- out - maybe later
    parse61 :: BS.ByteString -> Maybe (Day, Day, Amount)
    parse61 line = runST $ runMaybeT $ do
      lineRef <- newSTRef' $! BS.drop 4 line
      -- Subfeld 1: Valuta, 6f m n
      valuta <- readSTRef' lineRef >>= \x -> hoistMaybe $! parseTime' "%Y%m%d" $! C8.unpack $! BS.take 6 x
      modifySTRef' lineRef (BS.drop 6)

      -- Subfeld 2: Booking, 4f k n
      booking <- do
        x <- readSTRef' lineRef
        let bookingStr = BS.takeWhile (\c -> c >= 0x30 && c <= 0x39) x
        if BS.null bookingStr
          then return valuta -- No booking date, use same as valuta
          else do modifySTRef' lineRef (BS.drop 4)
                  hoistMaybe $! findBooking valuta bookingStr

      -- Subfeld 3: Credit/Debit, 2v m an: C (Credit), D (Debit), RC (Storno Credit), RD (Storno Debit)
      sign <- do
        x <- readSTRef' lineRef
        if (let y = BS.take 1 x in y == "C" || y == "D")
          then modifySTRef' lineRef (BS.drop 1) >> if BS.take 1 x == "C" then return 1 else return (-1)
          else if (let y = BS.take 2 x in y == "RC" || y == "RD")
               then modifySTRef' lineRef (BS.drop 2) >> if BS.take 2 x == "RC" then return (-1) else return 1
               else fail undefined

      -- Subfeld 4: Currency, 1f k an: last digit of 3 digit iso currency code
      -- I'll just skip this for now, it's always EUR anyway ...
      modifySTRef' lineRef (BS.drop 1)

      -- Subfeld 5: Betrag, 15v n (and ',') p
      amount <- do
        x <- readSTRef' lineRef
        let amtStr = BS.takeWhile (\c -> isDigit c || c == c2w ',') x
        modifySTRef' lineRef (BS.drop (BS.length amtStr))
        amt <- hoistMaybe $! readAmt x
        return $! Amount (sign*amt) EUR

      return (valuta, booking, amount)

    -- This is woefully incomplete and a lot more interesting stuff could be pulled
    -- out - maybe later
    parse86 :: BS.ByteString -> Maybe [T.Text]
    parse86 line = do
      sep <- if BS.length bs > 4 then return $! bs `BS.index` 4 else Nothing
      let items       = BS.split sep line
          isPurpose x = BS.length x > 1 && x `BS.index` 0 == c2w '2' && let y = x `BS.index` 1 in y >= c2w '0' && y <= c2w '9'
      return $! E.decodeLatin1 <$> BS.drop 2 <$> filter isPurpose items


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
                                                   ,("maxentries", DEentry (DEStr $! T.pack $! show maxEntries))])]

  -- FIXME: The format that the parser spits out should probably  be adjusted so that we
  -- can handle multiple Saldo results
  getResult (GetStatementList acntNum _ _ _ _) vals = do
    when (((acntNum ==) <$> (lookup "KUmsRes5.KTV.number" vals >>= deToTxt)) /= Just True) $!
      left $! HbciErrorInternal "Bank returned balance for wrong bank account: Currently only one account can be handled!"
    booked' <- fromMaybe (HbciErrorInternal "KUmsZeit5: Could not process booked entries") booked
    unbooked' <- fromMaybe (HbciErrorInternal "KUmsZeit5: Could not process unbooked entries") unbooked
    return $! GetStatementListResult booked' unbooked'
    where
      booked = parseMt94x =<< deToBS =<< lookup "KUmsZeit5.booked" vals
      unbooked = parseMt94x =<< deToBS =<< lookup "KUmsZeit5.unbooked" vals


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

parseTime' = parseTime defaultTimeLocale

formatTime' = formatTime defaultTimeLocale

fst3 (x,_,_) = x
