{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
module Data.HBCI.Parser where

import           Control.Monad (when)
import           Control.Monad.State (StateT, evalStateT, get, put, modify)
import           Control.Monad.Trans (lift)
import           Data.Either ()
import           Data.Monoid ((<>))
import           Data.Word
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import           Data.HBCI.Types

infixl 5 .@
(.@) :: BS.ByteString -> Int -> Word8
(.@) = BS.index

parseInt :: BS.ByteString -> Int -> (Int, Int)
parseInt bs i = go 0 i
  where
    n      = BS.length bs

    go l k = if n <= k
              then (l, k)
              else let x = bs .@ k
                   in if x >= 0x30 && x <= 0x39
                      then go (10*l + fromIntegral (x - 0x30)) (k+1)
                      else (l, k)

parseBinary :: BS.ByteString -> Int -> Either T.Text (DEValue, Int)
parseBinary bs i = do
  when (i >= BS.length bs) $ error "parseBinary: empty string"
  let (n, i') = parseInt bs i
  when (n == 0) $ error "parseBinary: binary length must not be 0"
  when (i' >= BS.length bs) $ error "parseBinary: illegal binary form"
  when (bs .@ i' /= 0x40) $ error "parseBinary: second '@' symbol missing"
  when (BS.length bs <= i'+1+n) $ error "parseBinary: string after '@' too short"
  return (DEBinary (BS.take n (BS.drop (i'+1) bs)), i'+1+n)

data PST = MkPST { msg :: MSGValue, seg :: SEGValue, deg :: DEGValue }

parser :: BS.ByteString -> Either T.Text MSGValue
parser bs = if BS.null bs then return [] else evalStateT (go [] 0) (MkPST [] [] [])
  where
    n = BS.length bs

    quote = 0x27
    plus  = 0x2B
    colon = 0x3A
    qmark = 0x3F
    at    = 0x40

    go :: [Word8] -> Int -> StateT PST (Either T.Text) MSGValue
    go stk i | bs .@ i == qmark = do ensureLength (i+2)
                                     go ((bs .@ i+1):stk) (i+2)
    go []  i | bs .@ i == at    = do (de, i') <- lift $ parseBinary bs (i+1)
                                     modify (\x -> x { deg = de : deg x})
                                     go [] i'
    go _   i | bs .@ i == at    = lift $ Left $ errorAt i <> " binary form within other element"
    go stk i | bs .@ i == colon = do ensureLength (i+1)
                                     modify (\x -> x { deg = addDE stk (deg x) })
                                     go [] (i+1)
    go stk i | bs .@ i == plus  = do ensureLength (i+1)
                                     modify (\x -> x { deg = [], seg = reverse (addDE stk (deg x)) : seg x })
                                     go [] (i+1)
    go stk i | bs .@ i == quote = do x <- get
                                     let msg' = reverse (reverse (addDE stk (deg x)) : seg x) : msg x
                                     if i+1 == n then return $ reverse msg'
                                       else do put $ MkPST { deg = [], seg = [], msg = msg' }
                                               go [] (i+1)
    go stk i                    = ensureLength (i+1) >> go ((bs .@ i) : stk) (i+1)

    errorAt i = "parse error at position " <> (T.pack $ show i)

    ensureLength i = when (i >= n) $ lift $ Left $ errorAt i  <> " string ended prematurely"

    hasBinHead ((DEBinary _):_) = True
    hasBinHead _                = False

    addDE stk@(_:_) des = mkDEStr stk : des
    addDE []        des = if hasBinHead des then des else mkDEStr [] : des

    mkDEStr = DEStr . E.decodeLatin1 . BS.pack . reverse
