{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.HBCI.Types
import Data.HBCI.Parser
import Data.HBCI.HbciDef

import Text.XML.Light (parseXML, onlyElems, Content(..))
import qualified Data.ByteString as BS

import Test.HUnit (assertBool)
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq expected actual = assertBool msg (expected == actual)
  where
    msg = "Expected: " ++ show expected ++ ", but got: " ++ show actual

parserTests :: [TF.Test]
parserTests = [ testGroup "Small known examples for HBCI message parsing"
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
          ],
          testGroup "Real HBCI messages"
          [ testCase "Encrypted message" $
            assertEq
            (Right [[[DEStr "HNHBK",DEStr "1",DEStr "3"],[DEStr "000000000369"],[DEStr "300"],[DEStr "0"],[DEStr "1"]],[[DEStr "HNVSK",DEStr "998",DEStr "3"],[DEStr "PIN",DEStr "1"],[DEStr "998"],[DEStr "1"],[DEStr "1",DEStr "",DEStr "0"],[DEStr "1",DEStr "20140626",DEStr "114832"],[DEStr "2",DEStr "2",DEStr "13",DEBinary "????????",DEStr "5",DEStr "1"],[DEStr "280",DEStr "20690500",DEStr "SomeUser",DEStr "V",DEStr "0",DEStr "0"],[DEStr "0"]],[[DEStr "HNVSD",DEStr "999",DEStr "1"],[DEBinary "HNSHK:2:4+PIN:1+999+844137429+1+1+1::0+1+1:20140626:114832+1:999:1+6:10:16+280:20690500:SomeUser:S:0:0'HKIDN:3:2+280:20690500+SomeUser+0+1'HKVVB:4:3+0+0+0+HBCI4Java+2.5'HKSYN:5:3+0'HNSHA:6:2+844137429++XXXXX'"]],[[DEStr "HNHBS",DEStr "7",DEStr "1"],[DEStr "1"]]])
            (parser "HNHBK:1:3+000000000369+300+0+1'HNVSK:998:3+PIN:1+998+1+1::0+1:20140626:114832+2:2:13:@8@????????:5:1+280:20690500:SomeUser:V:0:0+0'HNVSD:999:1+@208@HNSHK:2:4+PIN:1+999+844137429+1+1+1::0+1+1:20140626:114832+1:999:1+6:10:16+280:20690500:SomeUser:S:0:0'HKIDN:3:2+280:20690500+SomeUser+0+1'HKVVB:4:3+0+0+0+HBCI4Java+2.5'HKSYN:5:3+0'HNSHA:6:2+844137429++XXXXX''HNHBS:7:1+1'")
          ]
        ]

degDefTests :: [TF.Test]
degDefTests =
    [ testGroup "Constructed examples of DEGdefs"
      [ testCase "DEGdef 01" $
        assertEq (Just (DEGdef "01" True [] [])) (testF "<DEGdef id=\"01\" needsRequestTag=\"1\"/>")
      , testCase "DEGdef 02" $
        assertEq (Just (DEGdef "02" False [] [("a.b", "1"), ("c.d", "2")])) (testF "<DEGdef id=\"02\"><value path=\"a.b\">1</value><value path=\"c.d\">2</value></DEGdef>")
      , testCase "DEGdef 03" $
        assertEq (Just (DEGdef "03" False [DE "abcd" Code 5 (Just 99) 7 (Just 77) (Just ["a", "hello", "something"])] [("a.b", "1"), ("c.d", "2")]))
                 (testF "<DEGdef id=\"03\"><DE name=\"abcd\" type=\"Code\" minsize=\"5\" maxsize=\"99\" minnum=\"7\" maxnum=\"77\"/><valids path=\"abcd\"><validvalue>a</validvalue><validvalue>hello</validvalue><validvalue>something</validvalue></valids><value path=\"a.b\">1</value><value path=\"c.d\">2</value></DEGdef>")
      ]
    , testGroup "Known examples of DEGdefs"
      [ testCase "AllowedGV" $
        assertEq (Just (DEGdef { degId = "AllowedGV"
                               , degNeedsRequestTag = False
                               , degItems =
                                   [ DE "code"      AN  0 (Just 6) 1 Nothing Nothing
                                   , DE "reqSigs"   Num 0 (Just 2) 1 Nothing (Just ["0","1","2","3","98","99"])
                                   , DE "limittype" AN  0 (Just 1) 0 Nothing (Just ["E","T","W","M","Z"])
                                   , DE "value"     Wrt 0 Nothing  0 Nothing Nothing
                                   , DE "curr"      Cur 0 Nothing  0 Nothing Nothing
                                   , DE "limitdays" Num 0 (Just 3) 0 Nothing Nothing
                                   ]
                               , degValues = []
                               }))
        (testF "<DEGdef id=\"AllowedGV\"><DE name=\"code\" type=\"AN\" maxsize=\"6\"/><DE name=\"reqSigs\" type=\"Num\" maxsize=\"2\"/><DE name=\"limittype\" type=\"AN\" maxsize=\"1\" minnum=\"0\"/><DE name=\"value\" type=\"Wrt\" minnum=\"0\"/><DE name=\"curr\" type=\"Cur\" minnum=\"0\"/><DE name=\"limitdays\" type=\"Num\" maxsize=\"3\" minnum=\"0\"/><valids path=\"reqSigs\"><validvalue>0</validvalue><validvalue>1</validvalue><validvalue>2</validvalue><validvalue>3</validvalue><!-- Siehe http://www.onlinebanking-forum.de/phpBB2/viewtopic.php?t=14583 --><validvalue>98</validvalue><validvalue>99</validvalue></valids><valids path=\"limittype\"><validvalue>E</validvalue><validvalue>T</validvalue><validvalue>W</validvalue><validvalue>M</validvalue><validvalue>Z</validvalue></valids></DEGdef>")
             ]
           ]
    where
      testF :: BS.ByteString -> Maybe DEGdef
      testF = elemToDEGdef . head . onlyElems . parseXML

segDefTests :: [Content] -> [TF.Test]
segDefTests xml = []
    -- [ testGroup "Known examples of SEGdefs"
    --          [ testCase "BPA2" $
    --            assertEq (Just (SEGdef {segId = "BPA2", needsRequestTag = False, segDEsDEGs = [DE {deDef = DEdef {deName = "version", deType = "Num", deAttrs = [("maxsize","3")], deValids = Nothing}},DE {deDef = DEdef {deName = "kiname", deType = "AN", deAttrs = [("maxsize","60")], deValids = Nothing}},DE {deDef = DEdef {deName = "numgva", deType = "Num", deAttrs = [("maxsize","3")], deValids = Nothing}},DE {deDef = DEdef {deName = "maxmsgsize", deType = "Num", deAttrs = [("minnum","0"),("maxsize","4")], deValids = Nothing}}], values = [("SegHead.version","2"),("SegHead.code","HIBPA")]}))
    --                     (elemToSEGdef (getDEGs xml) $ head $ getSEGs xml)
    --          ]
    --        ]

sfDefTests :: [Content] -> [TF.Test]
sfDefTests xml = []
    -- [ testGroup "Known examples of SFdefs"
    --   [ testCase "UPD" $
    --     assertEq (Just (SFdef {}))
    --              (elemToSFdef (get))
    --   ]
    -- ]

msgDefTests :: [Content] -> [TF.Test]
msgDefTests xml = []

main :: IO ()
main = do
  xml <- getXml "resources/hbci-plus.xml"
  defaultMain $ parserTests ++ degDefTests ++ segDefTests xml ++ sfDefTests xml ++ msgDefTests xml
