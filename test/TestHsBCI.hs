{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import           Text.XML.Light (parseXML, onlyElems, Content(..))

import           Test.HUnit (assertBool)
import           Test.Framework as TF (defaultMain, testGroup, Test)
import           Test.Framework.Providers.HUnit

import           Data.HBCI.Types
import           Data.HBCI.Parser
import           Data.HBCI.HbciDef
import           Data.HBCI.Messages

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
          , testCase "Binary Form too short" $
            assertEq (Left "parseBinary: string after '@' too short") (parser "HNHBK:@5@???'")
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

elemToDETests :: [TF.Test]
elemToDETests =
  [ testGroup "elemToDETests"
    [ testCase "Minimal DE" $
      assertEq (Just $ DEdef "a" AN 0 Nothing 1 Nothing Nothing)
               (testF "<DE name=\"a\" type=\"AN\">")
    , testCase "Full DE" $
      assertEq (Just $ DEdef "HelloDe" DTAUS 1 (Just 2) 3 (Just 4) Nothing)
               (testF "<DE name=\"HelloDe\" type=\"DTAUS\" minsize=\"1\" maxsize=\"2\" minnum=\"3\" maxnum=\"4\">")
    , testCase "Maxnum 0 maps to Nothing" $
      assertEq (Just $ DEdef "HelloDe" DTAUS 1 (Just 2) 3 Nothing Nothing)
               (testF "<DE name=\"HelloDe\" type=\"DTAUS\" minsize=\"1\" maxsize=\"2\" minnum=\"3\" maxnum=\"0\">")
    , testCase "Name missing" $
      assertEq Nothing
               (testF "<DE type=\"AN\">")
    , testCase "Type missing" $
      assertEq Nothing
               (testF "<DE name=\"a\">")
    , testCase "Not a DE" $
      assertEq Nothing
               (testF "<DEG name=\"a\" type=\"AN\">")
    ]
  ]
  where
    testF :: BS.ByteString -> Maybe DE
    testF = elemToDE . head . onlyElems . parseXML

elemToValidsTests  :: [TF.Test]
elemToValidsTests =
  [ testGroup "elemToValidsTests"
    [ testCase "Empty valids" $
      assertEq (Just $ ("ref", []))
               (testF "<valids path=\"ref\"></valids>")
    , testCase "Three valids" $
      assertEq (Just $ ("ref", ["1","2","3"]))
               (testF "<valids path=\"ref\"><validvalue>1</validvalue><validvalue>2</validvalue><validvalue>3</validvalue></valids>")
    , testCase "path is missing" $
      assertEq Nothing
               (testF "<valids><validvalue>1</validvalue><validvalue>2</validvalue><validvalue>3</validvalue></valids>")
    , testCase "Not a valids" $
      assertEq Nothing
               (testF "<valid><validvalue>1</validvalue><validvalue>2</validvalue><validvalue>3</validvalue></valid>")
    ]
  ]
  where
    testF :: BS.ByteString -> Maybe (T.Text, [T.Text])
    testF = elemToValids . head . onlyElems . parseXML

elemToValueTests  :: [TF.Test]
elemToValueTests =
  [ testGroup "elemToValidsTests"
    [ testCase "Empty value" $
      assertEq (Just ("ref", ""))
               (testF "<value path=\"ref\"></value>")
    , testCase "Single value" $
      assertEq (Just ("ref", "1"))
               (testF "<value path=\"ref\">1</value>")
    , testCase "path is missing" $
      assertEq Nothing
               (testF "<value>1</value>")
    , testCase "Not a valids" $
      assertEq Nothing
               (testF "<values path=\"ref\">1</value>")
    ]
  ]
  where
    testF :: BS.ByteString -> Maybe (T.Text, T.Text)
    testF = elemToValue . head . onlyElems . parseXML

setDETests :: [TF.Test]
setDETests =
  [ testGroup "setDEG tests"
    [ testCase "Simple setValids test" $
      assertEq (DEdef "a" AN 0 Nothing 0 Nothing (Just ["1","2","3"]))
      (setDEValids "a" ["1","2","3"] (DEdef "a" AN 0 Nothing 0 Nothing Nothing))
    , testCase "setValids - names don't match" $
      assertEq (DEdef "a" AN 0 Nothing 0 Nothing Nothing)
      (setDEValids "b" ["1","2","3"] (DEdef "a" AN 0 Nothing 0 Nothing Nothing))
    , testCase "setValids - on a DEval" $
      assertEq (DEval (DEStr "1"))
      (setDEValids "b" ["1","2","3"] (DEval (DEStr "1")))
    , testCase "Simple setValue test" $
      assertEq (DEval (DEStr "1"))
      (setDEValue "a" "1" (DEdef "a" AN 0 Nothing 0 Nothing Nothing))
    ]
  ]

elemToDEGTests :: [TF.Test]
elemToDEGTests =
  [ testGroup "Constructed examples of DEGdefs"
    [ testCase "DEGdef 01" $
      assertEq (Just ("01", DEG "" 0 Nothing [])) (testF "<DEGdef id=\"01\" needsRequestTag=\"1\"/>")
    , testCase "DEGdef 02" $
      assertEq (Just ("02", DEG "" 0 Nothing [])) (testF "<DEGdef id=\"02\"><value path=\"a.b\">1</value><value path=\"c.d\">2</value></DEGdef>")
    , testCase "DEGdef 03" $
      assertEq (Just ("03", DEG "" 0  Nothing [DEdef "abcd" Code 5 (Just 99) 7 (Just 77) (Just ["a", "hello", "something"])]))
      (testF "<DEGdef id=\"03\"><DE name=\"abcd\" type=\"Code\" minsize=\"5\" maxsize=\"99\" minnum=\"7\" maxnum=\"77\"/><valids path=\"abcd\"><validvalue>a</validvalue><validvalue>hello</validvalue><validvalue>something</validvalue></valids><value path=\"a.b\">1</value><value path=\"c.d\">2</value></DEGdef>")
    ]
  , testGroup "Known examples of DEGdefs"
    [ testCase "AllowedGV" $
      assertEq (Just ("AllowedGV",
                      DEG "" 0 Nothing
                      [ DEdef "code"      AN  0 (Just 6) 1 Nothing Nothing
                      , DEdef "reqSigs"   Num 0 (Just 2) 1 Nothing (Just ["0","1","2","3","98","99"])
                      , DEdef "limittype" AN  0 (Just 1) 0 Nothing (Just ["E","T","W","M","Z"])
                      , DEdef "value"     Wrt 0 Nothing  0 Nothing Nothing
                      , DEdef "curr"      Cur 0 Nothing  0 Nothing Nothing
                      , DEdef "limitdays" Num 0 (Just 3) 0 Nothing Nothing
                      ]))
      (testF "<DEGdef id=\"AllowedGV\"><DE name=\"code\" type=\"AN\" maxsize=\"6\"/><DE name=\"reqSigs\" type=\"Num\" maxsize=\"2\"/><DE name=\"limittype\" type=\"AN\" maxsize=\"1\" minnum=\"0\"/><DE name=\"value\" type=\"Wrt\" minnum=\"0\"/><DE name=\"curr\" type=\"Cur\" minnum=\"0\"/><DE name=\"limitdays\" type=\"Num\" maxsize=\"3\" minnum=\"0\"/><valids path=\"reqSigs\"><validvalue>0</validvalue><validvalue>1</validvalue><validvalue>2</validvalue><validvalue>3</validvalue><!-- Siehe http://www.onlinebanking-forum.de/phpBB2/viewtopic.php?t=14583 --><validvalue>98</validvalue><validvalue>99</validvalue></valids><valids path=\"limittype\"><validvalue>E</validvalue><validvalue>T</validvalue><validvalue>W</validvalue><validvalue>M</validvalue><validvalue>Z</validvalue></valids></DEGdef>")
    ]
  ]
  where
    testF :: BS.ByteString -> Maybe (T.Text, DEG)
    testF = elemToDEG M.empty . head . onlyElems . parseXML

elemToSEGTests :: [TF.Test]
elemToSEGTests =
  [ testGroup "Constructed SEGdefs examples"
    [ testCase "Empty SEGdef" $
      assertEq (Just ("01", SEG "" False []))
      (testF1 "<SEGdef id=\"01\"></SEGdef>")
    , testCase "SEGdef with single DE" $
      assertEq (Just ("01", SEG "" False [DEItem (DEdef "de01" AN 0 Nothing 1 Nothing Nothing)]))
      (testF1 "<SEGdef id=\"01\"><DE name=\"de01\" type=\"AN\"/></SEGdef>")
    , testCase "SEGdef with single DEG" $
      assertEq (Just ("01", SEG "" False [DEGItem (DEG "" 1 (Just 1) [])]))
      (testF2 (M.fromList [("deg01", DEG "" 0 Nothing [])]) "<SEGdef id=\"01\"><DEG type=\"deg01\"/></SEGdef>")
    , testCase "SEGdef with single DEG" $
      assertEq (Just ("01", SEG "" False [DEGItem (DEG "DegName" 0 (Just 2) [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])]))
      (testF2 (M.fromList [("deg01", DEG "" 0 Nothing [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])])
       "<SEGdef id=\"01\"><DEG type=\"deg01\" name=\"DegName\" minnum=\"0\" maxnum=\"2\"/></SEGdef>")
    , testCase "SEGdef with single DEG and value" $
      assertEq (Just ("01", SEG "" False [DEGItem (DEG "DegName" 0 (Just 2) [DEval (DEStr "abcdefgh")])]))
      (testF2 (M.fromList [("deg01", DEG "DegName" 0 Nothing [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])])
       "<SEGdef id=\"01\"><DEG type=\"deg01\" name=\"DegName\" minnum=\"0\" maxnum=\"2\"/><value path=\"DegName.de01\">abcdefgh</value></SEGdef>")
    , testCase "SEGdef with single DEG and valids" $
      assertEq (Just ("01", SEG "" False [DEGItem (DEG "DegName" 0 (Just 2) [DEdef "de01" AN 0 Nothing 1 Nothing (Just ["a","b","c"])])]))
      (testF2 (M.fromList [("deg01", DEG "DegName" 0 Nothing [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])])
       "<SEGdef id=\"01\"><DEG type=\"deg01\" name=\"DegName\" minnum=\"0\" maxnum=\"2\"/><valids path=\"DegName.de01\"><validvalue>a</validvalue><validvalue>b</validvalue><validvalue>c</validvalue></valids></SEGdef>")
    , testCase "SEGdef with DEs and DEGs and values and valids" $
      assertEq
      (Just ("Seg01",
             SEG "" False
             [ DEItem (DEdef "de01" AN 0 Nothing 1 Nothing (Just ["de01-1","de01-2"]))
             , DEGItem (DEG "DegName02" 0 (Just 2) [DEdef "de02" AN 0 Nothing 1 Nothing (Just ["a","b","c"])])
             , DEItem (DEval (DEStr "de03val"))
             , DEGItem (DEG "DegName04" 0 (Just 5) [DEval (DEStr "de04-val")])
             ]))
      (testF2 (M.fromList [("deg02", DEG "" 0 Nothing [DEdef "de02" AN 0 Nothing 1 Nothing Nothing])
                          ,("deg04", DEG "" 0 Nothing [DEdef "de04" AN 0 Nothing 1 Nothing Nothing])])
       ("<SEGdef id=\"Seg01\">" <>
        "<DE name=\"de01\" type=\"AN\"/>" <>
        "<DEG type=\"deg02\" name=\"DegName02\" minnum=\"0\" maxnum=\"2\"/>" <>
        "<DE name=\"de03\" type=\"AN\"/>" <>
        "<DEG type=\"deg04\" name=\"DegName04\" minnum=\"0\" maxnum=\"5\"/>" <>
        "<valids path=\"de01\"><validvalue>de01-1</validvalue><validvalue>de01-2</validvalue></valids>" <>
        "<valids path=\"DegName02.de02\"><validvalue>a</validvalue><validvalue>b</validvalue><validvalue>c</validvalue></valids>" <>
        "<value path=\"de03\">de03val</value>" <>
        "<value path=\"DegName04.de04\">de04-val</value>" <>
        "</SEGdef>"))
    ]
  ]
  where
    testF1 :: BS.ByteString -> Maybe (T.Text, SEG)
    testF1 = elemToSEG M.empty . head . onlyElems . parseXML

    testF2 :: M.Map T.Text DEG -> BS.ByteString -> Maybe (T.Text, SEG)
    testF2 degs = elemToSEG degs . head . onlyElems . parseXML

  --   [ testCase "SEGdef01"
  --     assertEq Nothing
  --     (testF)
  --   ]
  -- ]
  -- [ testGroup "Known examples of SEGdefs"
  --   [ testCase "BPA2" $
  --     assertEq (Just (SEGdef {segId = "BPA2", needsRequestTag = False, segDEsDEGs = [DE {deDef = DEdef {deName = "version", deType = "Num", deAttrs = [("maxsize","3")], deValids = Nothing}},DE {deDef = DEdef {deName = "kiname", deType = "AN", deAttrs = [("maxsize","60")], deValids = Nothing}},DE {deDef = DEdef {deName = "numgva", deType = "Num", deAttrs = [("maxsize","3")], deValids = Nothing}},DE {deDef = DEdef {deName = "maxmsgsize", deType = "Num", deAttrs = [("minnum","0"),("maxsize","4")], deValids = Nothing}}], values = [("SegHead.version","2"),("SegHead.code","HIBPA")]}))
  --              (elemToSEGdef (getDEGdefs xml) $ head $ getSEGs xml)
  --   ]
  -- ]
--
-- sfDefTests :: [Content] -> [TF.Test]
-- sfDefTests xml = []
--     -- [ testGroup "Known examples of SFdefs"
--     --   [ testCase "UPD" $
--     --     assertEq (Just (SFdef {}))
--     --              (elemToSFdef (get))
--     --   ]
--     -- ]
--
-- msgDefTests :: [Content] -> [TF.Test]
-- msgDefTests xml = []
--
-- fillMsgTests :: [TF.Test]
-- fillMsgTests =
--   [ testGroup "Simple message examples"
--     [ testCase "Empty message" $
--       assertEq (Right [])
--                (fillMsg M.empty (MSGdef False False []))
--     , testCase "One item message -- success" $
--       assertEq (Right [[[DEStr "HNHBK"]]])
--                (fillMsg
--                 (M.fromList [("seg1.de1", "HNHBK")])
--                 (MSGdef False False [SEG "seg1" 1 Nothing (SEGdef False [DE "de1" AN 5 Nothing 1 (Just 1) Nothing])]))
--     , testCase "One item message -- failure" $
--       assertEq (Left "Key 'seg1.de1' missing in userVals")
--                (fillMsg
--                 M.empty
--                 (MSGdef False False [SEG "seg1" 1 Nothing (SEGdef False [DE "de1" AN 5 Nothing 1 (Just 1) Nothing])]))
--     , testCase "One item message -- value already set -- 1" $
--       assertEq (Right [[[DEStr "HNHBK"]]])
--                (fillMsg
--                 M.empty
--                 (MSGdef False False [SEG "seg1" 1 Nothing (SEGdef False [DEVal (DEStr "HNHBK")])]))
--     , testCase "One item message -- value already set -- 2" $
--       assertEq (Right [[[DEStr "HNHBK"]]])
--                (fillMsg
--                 (M.fromList [("seg1.de1", "SomethingOrOther")])
--                 (MSGdef False False [SEG "seg1" 1 Nothing (SEGdef False [DEVal (DEStr "HNHBK")])]))
--     , testCase "One item message -- value outside of valids" $
--       assertEq (Left "Value '3' for key 'seg1.de1' not in valid values '[\"1\",\"2\"]'")
--                (fillMsg
--                 (M.fromList [("seg1.de1", "3")])
--                 (MSGdef False False [SEG "seg1" 1 Nothing (SEGdef False [DE "de1" AN 5 Nothing 1 (Just 1) (Just ["1","2"])])]))
--     , testCase "Message with one DEG" $
--       assertEq (Right [[[DEStr "99", DEStr "77"]]])
--                (fillMsg
--                 (M.fromList [("seg1.deg1.de1", "99"), ("seg1.deg1.de2", "77" )])
--                 (MSGdef False False [SEG "seg1" 1 Nothing (SEGdef False
--                                                            [DEG "deg1" 0 Nothing (DEGdef [DE "de1" AN 5 Nothing 1 (Just 1) Nothing
--                                                                                                ,DE "de2" AN 9 Nothing 1 (Just 1) Nothing])])]))
--     , testCase "Message with one SF" $
--       assertEq (Right [[[DEStr "99", DEStr "77"]]])
--                (fillMsg
--                 (M.fromList [("seg1.deg1.de1", "99"), ("seg1.deg1.de2", "77" )])
--                 (MSGdef False False
--                  [SF "sf1" 0 Nothing
--                   (SFdef
--                    [SEG "seg1" 1 Nothing
--                     (SEGdef False
--                      [DEG "deg1" 0 Nothing
--                       (DEGdef
--                        [DE "de1" AN 5 Nothing 1 (Just 1) Nothing
--                        ,DE "de2" AN 9 Nothing 1 (Just 1) Nothing])])])]))
--     -- What else to test?
--     -- Validation of minsize, maxsize, minnum, maxnum
--     -- Validation of DETypes
--     -- Messages with SFs
--     -- Setting binary types
--     -- How does signing and encryption work? (I'll only do Pin-Tan for now)
--     -- First I need to validate the xml-message extraction functions and re-thing
--     -- the types there
--     ]
--   ]

standaloneTests :: [TF.Test]
standaloneTests = concat [ parserTests
                  , elemToDETests
                  , elemToValidsTests
                  , elemToValueTests
                  , setDETests
                  , elemToDEGTests
                  , elemToSEGTests
                  ]

xmlTests :: [[Content] -> TF.Test]
xmlTests = []

main :: IO ()
main = do
  hbciPlus <- getXml "resources/hbci-plus.xml"
  defaultMain (standaloneTests ++ map ($ hbciPlus) xmlTests)
