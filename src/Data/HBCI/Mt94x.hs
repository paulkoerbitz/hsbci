{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.Mt94x
       (parseMt94x)
       where

import           Control.Applicative ((<$>), (<*>), Applicative(..))
import           Control.Monad (liftM, ap)
import           Data.Monoid ((<>))
import           Data.Char (ord)
import           Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeLatin1)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Control.Monad.ST (runST, ST)
import           Control.Monad.Trans (lift, MonadTrans)
import           Data.STRef (newSTRef, readSTRef, modifySTRef, STRef)
import           Data.List (sort)
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Time.Format (parseTime)
import           Data.Time.Calendar (Day, toGregorian, diffDays)
import           System.Locale (defaultTimeLocale)

import           Data.HBCI.Types


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

instance (Functor f) => Functor (MaybeT f) where
  fmap f x = MaybeT $ fmap f' $ runMaybeT x
    where
      f' (Just y) = Just (f y)
      f' Nothing  = Nothing

instance (Functor m, Monad m) => Applicative (MaybeT m) where
  pure    = MaybeT . return . Just

  f <*> x = MaybeT $! do
    f' <- runMaybeT f
    case f' of
      Nothing -> return Nothing
      Just f'' -> do
        x' <- runMaybeT x
        case x' of
          Nothing -> return Nothing
          Just x'' -> return $! Just $! f'' x''

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
parseMt94x bs = process (chunks bs)

chunks bs = filter (\x -> ":61:" `BS.isPrefixOf` x || ":86:" `BS.isPrefixOf` x) $!
         snd $! foldr (\x (stk,l) -> if ":" `BS.isPrefixOf` x then ([], BS.intercalate "" (x:stk) : l) else (x:stk, l)) ([],[]) $!
         BS.takeWhile (/= c2w '\r') <$> C8.lines bs

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
  let eurStr = BS.take commaPos amtStr
      centStr = BS.takeWhile isDigit $! BS.drop (commaPos+1) amtStr
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
  valuta <- readSTRef' lineRef >>= \x -> hoistMaybe $! parseTime' "%Y%m%d" $! C8.unpack $! "20" <> BS.take 6 x
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
  line' <- return $! BS.drop 4 line
  sep <- if BS.length line' > 4 then return $! line' `BS.index` 3 else Nothing
  let items       = BS.split sep line'
      isPurpose x = BS.length x > 1 && x `BS.index` 0 == c2w '2' && let y = x `BS.index` 1 in y >= c2w '0' && y <= c2w '9'
  return $! E.decodeLatin1 <$> BS.drop 2 <$> filter isPurpose items

parseTime' :: String -> String -> Maybe Day
parseTime' = parseTime defaultTimeLocale
