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
          -- todo: I should also test that things fail as expected, but I'm too lazy for that right now
        ]

degDefTests :: [TF.Test]
degDefTests = [ testGroup "Known examples of DEGdefs"
             [ testCase "AllowedGV" $
               assertEq (Just (DEGdef {degId = "AllowedGV",
                                       degDEs = [ DEdef {deName = "code", deType = "AN", deAttrs = [("maxsize","6")], deValids = Nothing}
                                                , DEdef {deName = "reqSigs", deType = "Num", deAttrs = [("maxsize","2")], deValids = Just ["0","1","2","3","98","99"]}
                                                , DEdef {deName = "limittype", deType = "AN", deAttrs = [("minnum","0"),("maxsize","1")], deValids = Just ["E","T","W","M","Z"]}
                                                , DEdef {deName = "value", deType = "Wrt", deAttrs = [("minnum","0")], deValids = Nothing}
                                                , DEdef {deName = "curr", deType = "Cur", deAttrs = [("minnum","0")], deValids = Nothing}
                                                , DEdef {deName = "limitdays", deType = "Num", deAttrs = [("minnum","0"),("maxsize","3")], deValids = Nothing}]}))
                        (elemToDEGdef $ head $ onlyElems $ parseXML ("<DEGdef id=\"AllowedGV\"><DE name=\"code\" type=\"AN\" maxsize=\"6\"/><DE name=\"reqSigs\" type=\"Num\" maxsize=\"2\"/><DE name=\"limittype\" type=\"AN\" maxsize=\"1\" minnum=\"0\"/><DE name=\"value\" type=\"Wrt\" minnum=\"0\"/><DE name=\"curr\" type=\"Cur\" minnum=\"0\"/><DE name=\"limitdays\" type=\"Num\" maxsize=\"3\" minnum=\"0\"/><valids path=\"reqSigs\"><validvalue>0</validvalue><validvalue>1</validvalue><validvalue>2</validvalue><validvalue>3</validvalue><!-- Siehe http://www.onlinebanking-forum.de/phpBB2/viewtopic.php?t=14583 --><validvalue>98</validvalue><validvalue>99</validvalue></valids><valids path=\"limittype\"><validvalue>E</validvalue><validvalue>T</validvalue><validvalue>W</validvalue><validvalue>M</validvalue><validvalue>Z</validvalue></valids></DEGdef>" :: BS.ByteString))
             ]
           ]

segDefTests :: [Content] -> [TF.Test]
segDefTests xml =
    [ testGroup "Known examples of SEGdefs"
             [ testCase "BPA2" $
               assertEq (Just (SEGdef {segId = "BPA2", needsRequestTag = False, segDEsDEGs = [DE {deDef = DEdef {deName = "version", deType = "Num", deAttrs = [("maxsize","3")], deValids = Nothing}},DE {deDef = DEdef {deName = "kiname", deType = "AN", deAttrs = [("maxsize","60")], deValids = Nothing}},DE {deDef = DEdef {deName = "numgva", deType = "Num", deAttrs = [("maxsize","3")], deValids = Nothing}},DE {deDef = DEdef {deName = "maxmsgsize", deType = "Num", deAttrs = [("minnum","0"),("maxsize","4")], deValids = Nothing}}], values = [("SegHead.version","2"),("SegHead.code","HIBPA")]}))
                        (elemToSEGdef (getDEGs xml) $ head $ getSEGs xml)
             ]
           ]

sfDefTests :: [Content] -> [TF.Test]
sfDefTests xml =
    [ testGroup "Known examples of SFdefs"
      [ testCase "UPD" $
        assertEq (Just (SFdef {}))
                 (elemToSFdef (get))
      ]
    ]

msgDefTests :: [Content] -> [TF.Test]
msgDefTests xml = []

main :: IO ()
main = do
  xml <- getXml "src/hbci-plus.xml"
  defaultMain $ parserTests ++ degDefTests ++ segDefTests xml ++ sfDefTests xml ++ msgDefTests xml
