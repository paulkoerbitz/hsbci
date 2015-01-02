{-# LANGUAGE OverloadedStrings #-}
module Data.HBCI.Utils where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans.Either (EitherT, left, right, hoistEither)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import           Data.HBCI.Types

fromMaybe :: Monad m => HbciError -> Maybe a -> EitherT HbciError m a
fromMaybe e m = maybe (left e) right m

fromEither :: Monad m => Either T.Text a -> EitherT HbciError m a
fromEither = hoistEither . onLeft HbciErrorOther

onLeft :: (a -> b) -> Either a c -> Either b c
onLeft l (Left  x)  = Left (l x)
onLeft _ (Right x)  = Right x

onRight :: (a -> b) -> Either c a -> Either c b
onRight _ (Left  x)  = Left x
onRight r (Right x)  = Right (r x)

deToTxt :: DEValue -> Maybe T.Text
deToTxt (DEStr x) = Just x
deToTxt _         = Nothing

readInt :: T.Text -> Maybe Int
readInt x = case TR.decimal x of
  Right (i, _) -> Just i
  _ -> Nothing

deToInt :: DEValue -> Maybe Int
deToInt x = deToTxt x >>= readInt

btgToInt :: DEValue -> Maybe Int
btgToInt x = do
  txt <- deToTxt x
  case TR.decimal txt of
    Right (euros, rest) -> (euros * 100 +) <$> toCents rest
    _ -> Nothing
  where
    toCents txt | txt == "" || txt == "," = Just 0
    toCents txt                           = case TR.decimal (T.drop 1 txt) of
      Right (cents, _) -> Just cents
      _                -> Nothing
