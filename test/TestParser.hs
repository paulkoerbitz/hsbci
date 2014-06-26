{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.HBCI.Types
import Data.HBCI.Parser

import Test.HUnit (assertBool)
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq expected actual = assertBool msg (expected == actual)
  where
    msg = "Expected: " ++ show expected ++ ", but got: " ++ show actual

tests :: [TF.Test]
tests = [ testGroup "Small known examples for HBCI message parsing"
          [ testCase "Empty message" $ assertEq (Right []) (parser "")
          , testCase "Single segment" $
            assertEq (Right [[[DEStr "HNHBK", DEStr "1", DEStr "3", DEStr "1234123"]]])
                     (parser "HNHBK:1:3:1234123'")
          , testCase "Binary Form" $
            assertEq (Right [[[DEStr "HNHBK", DEBinary "?????"]]]) (parser "HNHBK:@5@?????'")
          , testCase "Questionmark Escape" $
            assertEq (Right [[[DEStr "HNHBK", DEStr "?@:+'"]]]) (parser "HNHBK:???@?:?+?''")
          , testCase "Empty DEs1" $
            assertEq (Right [[[DEStr "", DEStr ""]]]) (parser ":'")
          , testCase "Empty DEs2" $
            assertEq (Right [[[DEStr "", DEStr "", DEStr ""], [DEStr ""], [DEStr ""]]]) (parser "::++'")
          , testCase "Empty DEs3" $
            assertEq (Right [[[DEStr "", DEStr ""], [DEStr ""]], [[DEStr ""], [DEStr "", DEStr ""]]])
                     (parser ":+'+:'")
          , testCase "Two Segments" $
            assertEq (Right [[[DEStr "HNHBK", DEStr "1"], [DEStr "2"]], [[DEStr "HNHBS"], [DEStr "3", DEStr "4"]]])
                     (parser "HNHBK:1+2'HNHBS+3:4'")
          ]
          -- todo: I should also test that things fail as expected, but I'm too lazy for that right now
        ]

main :: IO ()
main = defaultMain tests
