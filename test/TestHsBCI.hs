{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Applicative ((<$>))
import           Control.Arrow (second)
import           Control.Monad.State (evalStateT)
import           Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import           Text.XML.Light (parseXML, onlyElems, Content(..))
import           Text.PrettyPrint (render)

import           Test.HUnit (assertBool)
import           Test.Framework as TF (defaultMain, testGroup, Test)
import           Test.Framework.Providers.HUnit

import           Data.HBCI.Types
import           Data.HBCI.Parser
import           Data.HBCI.HbciDef
import           Data.HBCI.Messages
import           Data.HBCI.Gen

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq expected actual = assertBool msg (expected == actual)
  where
    msg = "Expected: " ++ show expected ++ ", but got: " ++ show actual

assertEqPretty :: (Eq a, HbciPretty a) => a -> a -> IO ()
assertEqPretty expected actual = assertBool msg (expected == actual)
  where
    msg = "Expected:\n" ++ show (toDoc expected) ++ "\n\nbut got:\n" ++ show (toDoc actual)

missingTestCase :: IO ()
missingTestCase =
  assertBool "Missing test case -- expected failure" False

parserTests :: [TF.Test]
parserTests =
  [ testGroup "Small known examples for HBCI message parsing"
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
    ]
  , testGroup "Real HBCI messages"
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
      assertEq (Right $ DEdef "a" AN 0 Nothing 1 Nothing Nothing)
               (testF "<DE name=\"a\" type=\"AN\">")
    , testCase "Full DE" $
      assertEq (Right $ DEdef "HelloDe" DTAUS 1 (Just 2) 3 (Just 4) Nothing)
               (testF "<DE name=\"HelloDe\" type=\"DTAUS\" minsize=\"1\" maxsize=\"2\" minnum=\"3\" maxnum=\"4\">")
    , testCase "Maxnum 0 maps to Nothing" $
      assertEq (Right $ DEdef "HelloDe" DTAUS 1 (Just 2) 3 Nothing Nothing)
               (testF "<DE name=\"HelloDe\" type=\"DTAUS\" minsize=\"1\" maxsize=\"2\" minnum=\"3\" maxnum=\"0\">")
    , testCase "Name missing" $
      assertEq (Left "1: Required attribute 'name' missing in DE")
               (testF "<DE type=\"AN\">")
    , testCase "Type missing" $
      assertEq (Left "1: Required attribute 'type' missing in DE")
               (testF "<DE name=\"a\">")
    , testCase "Not a DE" $
      assertEq (Left "1: Element is not a DE: DEG")
               (testF "<DEG name=\"a\" type=\"AN\">")
    ]
  ]
  where
    testF :: BS.ByteString -> Either T.Text DE
    testF = elemToDE . head . onlyElems . parseXML

elemToValidsTests  :: [TF.Test]
elemToValidsTests =
  [ testGroup "elemToValidsTests"
    [ testCase "Empty valids" $
      assertEq (Right ("ref", []))
               (testF "<valids path=\"ref\"></valids>")
    , testCase "Three valids" $
      assertEq (Right ("ref", ["1","2","3"]))
               (testF "<valids path=\"ref\"><validvalue>1</validvalue><validvalue>2</validvalue><validvalue>3</validvalue></valids>")
    , testCase "path is missing" $
      assertEq (Left "1: Required attribute 'path' missing in valids")
               (testF "<valids><validvalue>1</validvalue><validvalue>2</validvalue><validvalue>3</validvalue></valids>")
    , testCase "Not a valids" $
      assertEq (Left "1: Element is not a valids: valid")
               (testF "<valid><validvalue>1</validvalue><validvalue>2</validvalue><validvalue>3</validvalue></valid>")
    ]
  ]
  where
    testF :: BS.ByteString -> Either T.Text (T.Text, [T.Text])
    testF = elemToValids . head . onlyElems . parseXML

elemToValueTests  :: [TF.Test]
elemToValueTests =
  [ testGroup "elemToValidsTests"
    [ testCase "Empty value" $
      assertEq (Right ("ref", ""))
               (testF "<value path=\"ref\"></value>")
    , testCase "Single value" $
      assertEq (Right ("ref", "1"))
               (testF "<value path=\"ref\">1</value>")
    , testCase "path is missing" $
      assertEq (Left "1: Required attribute 'path' missing in value")
               (testF "<value>1</value>")
    , testCase "Not a valids" $
      assertEq (Left "1: Element is not a value: values")
               (testF "<values path=\"ref\">1</value>")
    ]
  ]
  where
    testF :: BS.ByteString -> Either T.Text (T.Text, T.Text)
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
      assertEq (Right ("01", DEG "" 0 Nothing [])) (testF "<DEGdef id=\"01\" needsRequestTag=\"1\"/>")
    , testCase "DEGdef 02" $
      assertEq (Right ("02", DEG "" 0 Nothing [])) (testF "<DEGdef id=\"02\"><value path=\"a.b\">1</value><value path=\"c.d\">2</value></DEGdef>")
    , testCase "DEGdef 03" $
      assertEq (Right ("03", DEG "" 0  Nothing [DEdef "abcd" Code 5 (Just 99) 7 (Just 77) (Just ["a", "hello", "something"])]))
      (testF "<DEGdef id=\"03\"><DE name=\"abcd\" type=\"Code\" minsize=\"5\" maxsize=\"99\" minnum=\"7\" maxnum=\"77\"/><valids path=\"abcd\"><validvalue>a</validvalue><validvalue>hello</validvalue><validvalue>something</validvalue></valids><value path=\"a.b\">1</value><value path=\"c.d\">2</value></DEGdef>")
    ]
  , testGroup "Known examples of DEGdefs"
    [ testCase "AllowedGV" $
      assertEq (Right ("AllowedGV",
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
    -- FIXME: Test failures
  ]
  where
    testF :: BS.ByteString -> Either T.Text (T.Text, DEG)
    testF = elemToDEG M.empty . head . onlyElems . parseXML

elemToSEGTests :: [TF.Test]
elemToSEGTests =
  [ testGroup "Constructed SEGdefs examples"
    [ testCase "Empty SEGdef" $
      assertEq (Right ("01", SEG "" False []))
      (testF1 "<SEGdef id=\"01\"></SEGdef>")
    , testCase "SEGdef with single DE" $
      assertEq (Right ("01", SEG "" False [DEItem (DEdef "de01" AN 0 Nothing 1 Nothing Nothing)]))
      (testF1 "<SEGdef id=\"01\"><DE name=\"de01\" type=\"AN\"/></SEGdef>")
    , testCase "SEGdef with single DEG" $
      assertEq (Right ("01", SEG "" False [DEGItem (DEG "" 1 (Just 1) [])]))
      (testF2 (M.fromList [("deg01", DEG "" 0 Nothing [])]) "<SEGdef id=\"01\"><DEG type=\"deg01\"/></SEGdef>")
    , testCase "SEGdef with single DEG" $
      assertEq (Right ("01", SEG "" False [DEGItem (DEG "DegName" 0 (Just 2) [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])]))
      (testF2 (M.fromList [("deg01", DEG "" 0 Nothing [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])])
       "<SEGdef id=\"01\"><DEG type=\"deg01\" name=\"DegName\" minnum=\"0\" maxnum=\"2\"/></SEGdef>")
    , testCase "SEGdef with single DEG and value" $
      assertEq (Right ("01", SEG "" False [DEGItem (DEG "DegName" 0 (Just 2) [DEval (DEStr "abcdefgh")])]))
      (testF2 (M.fromList [("deg01", DEG "DegName" 0 Nothing [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])])
       "<SEGdef id=\"01\"><DEG type=\"deg01\" name=\"DegName\" minnum=\"0\" maxnum=\"2\"/><value path=\"DegName.de01\">abcdefgh</value></SEGdef>")
    , testCase "SEGdef with single DEG and valids" $
      assertEq (Right ("01", SEG "" False [DEGItem (DEG "DegName" 0 (Just 2) [DEdef "de01" AN 0 Nothing 1 Nothing (Just ["a","b","c"])])]))
      (testF2 (M.fromList [("deg01", DEG "DegName" 0 Nothing [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])])
       "<SEGdef id=\"01\"><DEG type=\"deg01\" name=\"DegName\" minnum=\"0\" maxnum=\"2\"/><valids path=\"DegName.de01\"><validvalue>a</validvalue><validvalue>b</validvalue><validvalue>c</validvalue></valids></SEGdef>")
    , testCase "SEGdef with DEs and DEGs and values and valids" $
      assertEq
      (Right ("Seg01",
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
    -- FIXME: Test failures
  ]
  where
    testF1 :: BS.ByteString -> Either T.Text (T.Text, SEG)
    testF1 = elemToSEG M.empty . head . onlyElems . parseXML

    testF2 :: M.Map T.Text DEG -> BS.ByteString -> Either T.Text (T.Text, SEG)
    testF2 degs = elemToSEG degs . head . onlyElems . parseXML

elemToSFTests :: [TF.Test]
elemToSFTests =
  [ testGroup "SF Tests"
    [ testCase "Not a SF" $
      assertEq (Left "1: Element is not a SFdef: SEGdef")
      (testF "<SEGdef id=\"sf01\"></SEGdef>")
    , testCase "Empty SF" $
      assertEq (Right ("sf01", []))
      (testF "<SFdef id=\"sf01\"></SFdef>")
    , testCase "Single Seg in SF" $
      assertEq (Right ("sf01", [SF 5 (Just 7) [SEG "segName01" False []]]))
      (testF2 (M.fromList [("segId01", SEG "" False [])]) M.empty
       "<SFdef id=\"sf01\"><SEG type=\"segId01\" name=\"segName01\" minnum=\"5\" maxnum=\"7\"/></SFdef>")
    , testCase "Two Segs in SF" $
      assertEq (Right ("sf01", [SF 5 (Just 7) [SEG "segName01" False []], SF 1 (Just 1) [SEG "segName02" False []]]))
      (testF2 (M.fromList [("segId01", SEG "" False []),("segId02", SEG "" False [])]) M.empty
       "<SFdef id=\"sf01\"><SEG type=\"segId01\" name=\"segName01\" minnum=\"5\" maxnum=\"7\"/><SEG type=\"segId02\" name=\"segName02\"/></SFdef>")
    , testCase "Single SF in SFdef" $
      assertEq (Right ("sf01", [SF 0 Nothing [SEG "seg01" False [], SEG "seg02" False []]]))
      (testF2 M.empty (M.fromList [("sfId01", [SF 0 Nothing [SEG "seg01" False [], SEG "seg02" False []]])])
       "<SFdef id=\"sf01\"><SF type=\"sfId01\"/></SFdef>")
    ]
  ]
  where
    testF :: BS.ByteString -> Either T.Text (T.Text, [SF])
    testF = elemToSF M.empty M.empty . head . onlyElems . parseXML

    testF2 :: M.Map T.Text SEG -> M.Map T.Text [SF] -> BS.ByteString -> Either T.Text (T.Text, [SF])
    testF2 segs sfs = elemToSF segs sfs . head . onlyElems . parseXML

elemToMSGTests :: [TF.Test]
elemToMSGTests =
  [ testGroup "MSG Tests"
    [ testCase "Not a MSGdef" $
      assertEq (Left "1: Element is not a MSGdef: SFdef")
      (testF M.empty M.empty "<SFdef id=\"msg01\"></SFdef>")
    , testCase "Empty MSGdef" $
      assertEq (Right ("msg01", MSG True True []))
      (testF M.empty M.empty "<MSGdef id=\"msg01\"></MSGdef>")
    , testCase "MSGdef dontCrypt" $
      assertEq (Right ("msg01", MSG True False []))
      (testF M.empty M.empty "<MSGdef id=\"msg01\" dontcrypt=\"1\"></MSGdef>")
    , testCase "MSGdef dontSign" $
      assertEq (Right ("msg01", MSG False True []))
      (testF M.empty M.empty "<MSGdef id=\"msg01\" dontsign=\"1\"></MSGdef>")
    , testCase "MSGdef with single SF which doesn't exit in the dicts" $
      assertEq (Left "1: SF: Referenced element 'sf01' not found")
      (testF M.empty M.empty "<MSGdef id=\"msg01\"><SF type=\"sf01\"/></MSGdef>")
    , testCase "MSGdef with single SF, minnum, maxnum unspecified" $
      assertEq (Right ("msg01", MSG True True [SF 1 (Just 1) []]))
      (testF M.empty (M.fromList [("sf01", [SF 1 (Just 1) []])]) "<MSGdef id=\"msg01\"><SF type=\"sf01\"/></MSGdef>")
    , testCase "MSGdef with single SEG which doesn't exit in the dicts" $
      assertEq (Left "1: SEG: Referenced element 'seg01' not found")
      (testF M.empty M.empty "<MSGdef id=\"msg01\"><SEG type=\"seg01\"/></MSGdef>")
    , testCase "MSGdef with single SEG, minnum, maxnum unspecified" $
      assertEq (Right ("msg01", MSG True True [SF 1 (Just 1) [SEG "" False []]]))
      (testF (M.fromList [("seg01", SEG "" False [])]) M.empty "<MSGdef id=\"msg01\"><SEG type=\"seg01\"/></MSGdef>")
    , testCase "MSGdef with single SEG, name, minnum, and maxnum" $
      assertEq (Right ("msg01", MSG True True [SF 0 (Just 99) [SEG "SegName01" False []]]))
      (testF (M.fromList [("seg01", SEG "" False [])]) M.empty "<MSGdef id=\"msg01\"><SEG name=\"SegName01\" minnum=\"0\" maxnum=\"99\" type=\"seg01\"/></MSGdef>")
    , testCase "MSGdef with SF containing two SEGs and a SEG" $
      let segMap = M.fromList [("seg01", SEG "" False [])]
          sfMap = M.fromList [("sf01", [SF 5 (Just 20) []])]
      in assertEq (Right ("msg01", MSG True True [SF 3 Nothing [], SF 0 (Just 99) [SEG "SegName01" False []]]))
         (testF segMap sfMap
          ("<MSGdef id=\"msg01\">" <>
           "<SF type=\"sf01\" minnum=\"3\" maxnum=\"0\"/>" <>
           "<SEG name=\"SegName01\" minnum=\"0\" maxnum=\"99\" type=\"seg01\"/>" <>
           "</MSGdef>"))
    , testCase "MSGdef with SF containing two SEGs and a SEG and values" $
      let segMap = M.fromList [("seg02", SEG "" False [DEItem (DEdef "DeInSeg" AN 0 Nothing 0 Nothing Nothing)])]
          sfMap = M.fromList [("sf01", [SF 1 (Just 20) [SEG "SegInSf" False [DEGItem (DEG "DegInSf" 0 Nothing [DEdef "DeInSf" AN 0 Nothing 0 Nothing Nothing])]]])]
      in assertEq (Right ("msg01", MSG True True [SF 1 Nothing [SEG "SegInSf" False
                                                               [DEGItem (DEG "DegInSf" 0 Nothing [DEval (DEStr "123")])]]
                                                ,SF 0 (Just 99) [SEG "SegOnTop" False [DEItem (DEval (DEStr "456"))]]
                                                ]))
         (testF segMap sfMap
          ("<MSGdef id=\"msg01\">" <>
           "<SF type=\"sf01\" minnum=\"3\" maxnum=\"0\"/>" <>
           "<SEG name=\"SegOnTop\" minnum=\"0\" maxnum=\"99\" type=\"seg02\"/>" <>
           "<value path=\"SegInSf.DegInSf.DeInSf\">123</value>" <>
           "<value path=\"SegOnTop.DeInSeg\">456</value>" <>
           "</MSGdef>"))
    ]
  ]
  where
    testF :: M.Map T.Text SEG -> M.Map T.Text [SF] -> BS.ByteString -> Either T.Text (T.Text, MSG)
    testF segs sfs = elemToMSG segs sfs . head . onlyElems . parseXML


getMSGfromXMLTest :: [[Content] -> TF.Test]
getMSGfromXMLTest =
  [ \xml -> testGroup "getMSGfromXMLTest"
    [ testCase "DialogInitAnon" $
      assertEq
      (Right (Just dialogInitAnon))
      (getMSGfromXML xml >>= return . M.lookup "DialogInitAnon")
    , testCase "DialogInitAnonRes" $
      assertEqPretty
      (Just dialogInitAnonRes)
      (either undefined id $ getMSGfromXML xml >>= return . M.lookup "DialogInitAnonRes")
    ]
  ]

fillDeTests :: [TF.Test]
fillDeTests =
  [ testGroup "fillDeTests - simple DE examples"
    [ testCase "DEStr is not modified" $
      assertEq (Right (DEStr "abc"))
      (testF M.empty "" (DEval (DEStr "abc")))
    , testCase "DEBinary is not modified" $
      assertEq (Right (DEBinary "abc"))
      (testF M.empty "" (DEval (DEBinary "abc")))
    , testCase "Simple replacement" $
      assertEq (Right (DEStr "abc"))
      (testF (M.fromList [("deKey", DEStr "abc")]) "" (DEdef "deKey" AN 0 Nothing 0 Nothing Nothing))
    , testCase "Replacement with prefix" $
      assertEq (Right (DEStr "abc"))
      (testF (M.fromList [("prefix.suffix.deKey", DEStr "abc")]) "prefix.suffix" (DEdef "deKey" AN 0 Nothing 0 Nothing Nothing))
    , testCase "Names with multiple dots are found" $
      assertEq (Right (DEStr "abc"))
      (testF (M.fromList [("prefix.suffix.deKey0.deKey1", DEStr "abc")]) "prefix.suffix" (DEdef "deKey0.deKey1" AN 0 Nothing 0 Nothing Nothing))
    , testCase "fillDe gives error if name is outside of valids" $
      assertEq (Left "Value 'abc' for key 'deKey' not in valid values '[\"ab\",\"c\",\"ac\"]'")
      (testF (M.fromList [("deKey", DEStr "abc")]) "" (DEdef "deKey" AN 0 Nothing 0 Nothing (Just ["ab", "c", "ac"])))
    , testCase "AN replacement results in string" $
      assertEq (Right (DEStr "abc0123"))
      (testF (M.fromList [("deKey", DEStr "abc0123")]) "" (DEdef "deKey" AN 0 Nothing 0 Nothing Nothing))
    , testCase "AN replacement escapes ?@':+ with ?" $
      assertEq (Right (DEStr "???@?'?:?+"))
      (testF (M.fromList [("deKey", DEStr "?@':+")]) "" (DEdef "deKey" AN 0 Nothing 0 Nothing Nothing))
    , testCase "Bin replacement results in binary" $
      assertEq (Right (DEBinary "?@'"))
      (testF (M.fromList [("deKey", DEBinary "?@'")]) "" (DEdef "deKey" Bin 0 Nothing 0 Nothing Nothing))
    , testCase "Code replacement results in string" $
      assertEq (Right (DEStr "123456"))
      (testF (M.fromList [("deKey", DEStr "123456")]) "" (DEdef "deKey" Code 0 Nothing 0 Nothing Nothing))
    , testCase "Ctr replacement results in string" $
      assertEq (Right (DEStr "123456"))
      (testF (M.fromList [("deKey", DEStr "123456")]) "" (DEdef "deKey" Ctr 0 Nothing 0 Nothing Nothing))
    , testCase "Cur replacement results in string" $
      assertEq (Right (DEStr "EUR"))
      (testF (M.fromList [("deKey", DEStr "EUR")]) "" (DEdef "deKey" Cur 0 Nothing 0 Nothing Nothing))
    , testCase "DTAUS replacement results in binary" $
      assertEq (Right (DEBinary "SomethingSomething:?@'"))
      (testF (M.fromList [("deKey", DEBinary "SomethingSomething:?@'")]) "" (DEdef "deKey" DTAUS 0 Nothing 0 Nothing Nothing))
    , testCase "Date replacement results in string" $
      assertEq (Right (DEStr "24.07.2014"))
      (testF (M.fromList [("deKey", DEStr "24.07.2014")]) "" (DEdef "deKey" Date 0 Nothing 0 Nothing Nothing))
    , testCase "Dig replacement results in string" $
      assertEq (Right (DEStr "0123456789"))
      (testF (M.fromList [("deKey", DEStr "0123456789")]) "" (DEdef "deKey" Dig 0 Nothing 0 Nothing Nothing))
    , testCase "ID replacement results in string" $
      assertEq (Right (DEStr "0123456789"))
      (testF (M.fromList [("deKey", DEStr "0123456789")]) "" (DEdef "deKey" ID 0 Nothing 0 Nothing Nothing))
    , testCase "JN replacement results in string" $
      assertEq (Right (DEStr "J"))
      (testF (M.fromList [("deKey", DEStr "J")]) "" (DEdef "deKey" JN 0 Nothing 0 Nothing Nothing))
    , testCase "Num replacement results in string" $
      assertEq (Right (DEStr "0123456789"))
      (testF (M.fromList [("deKey", DEStr "0123456789")]) "" (DEdef "deKey" Num 0 Nothing 0 Nothing Nothing))
    , testCase "Time replacement results in string" $
      assertEq (Right (DEStr "12?:13?:14"))
      (testF (M.fromList [("deKey", DEStr "12:13:14")]) "" (DEdef "deKey" Time 0 Nothing 0 Nothing Nothing))
    , testCase "Wrt replacement results in string" $
      assertEq (Right (DEStr "123,45"))
      (testF (M.fromList [("deKey", DEStr "123,45")]) "" (DEdef "deKey" Time 0 Nothing 0 Nothing Nothing))
    , testCase "fillDe gives error if provided string too long" $
      assertEq (Left "Field 'deKey' has a maxsize of 5 but provided value '123456' has a length of 6")
      (testF (M.fromList [("deKey", DEStr "123456")]) "" (DEdef "deKey" AN 0 (Just 5) 0 Nothing Nothing))
    , testCase "fillDe fills entry with 0s if provided Num too short" $
      assertEq (Right (DEStr "000123"))
      (testF (M.fromList [("deKey", DEStr "123")]) "" (DEdef "deKey" Num 6 (Just 6) 0 Nothing Nothing))
    , testCase "fillDe gives error if values of other types are too short" $
      assertEq (Left "Field 'deKey' has a minsize of 6 but provided value '123' has a length of 3")
      (testF (M.fromList [("deKey", DEStr "123")]) "" (DEdef "deKey" AN 6 (Just 6) 0 Nothing Nothing))
    ]
  ]
  where
    testF x y z= evalStateT (fillDe x y z) (MkFillState 0 1)


fillMsgTests :: [TF.Test]
fillMsgTests =
  [ testGroup "Simple message examples"
    [ testCase "One item message -- success" $
      assertEq (Right [[[DEStr "HNHBK"],[DEStr "000000000019"]]])
               (fillMsg
                (M.fromList [("MsgHead.de1", DEStr "HNHBK")])
                (MSG False False [SF 0 Nothing [SEG "MsgHead" False [DEItem (DEdef "de1" AN 5 Nothing 1 (Just 1) Nothing)
                                                                    ,DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 (Just 1) Nothing)]]]))
    , testCase "One item message -- missing item" $
      assertEq (Left "Required key 'seg1.de1' missing in userVals")
               (fillMsg
                M.empty
                (MSG False False [SF 0 Nothing [SEG "seg1" False [DEItem (DEdef "de1" AN 5 Nothing 1 (Just 1) Nothing)]]]))
    , testCase "One item message -- missing msgsize field" $
      assertEq (Left "Didn't find expected field message size")
               (fillMsg
                (M.fromList [("seg1.de1", DEStr "abcxyz")])
                (MSG False False [SF 0 Nothing [SEG "seg1" False [DEItem (DEdef "de1" AN 5 Nothing 1 (Just 1) Nothing)]]]))
    , testCase "One item message -- value already set 1" $
      assertEq (Right [[[DEStr "HNHBK"],[DEStr "000000000019"]]])
               (fillMsg
                M.empty
                (MSG False False [SF 0 Nothing [SEG "MsgHead" False [DEItem (DEval (DEStr "HNHBK"))
                                                                    ,DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 (Just 1) Nothing)]]]))
    , testCase "One item message -- value already set 2" $
      assertEq (Right [[[DEStr "HNHBK"],[DEStr "000000000019"]]])
               (fillMsg
                (M.fromList [("MsgHead.de1", DEStr "SomethingOrOther")])
                (MSG False False [SF 0 Nothing [SEG "MsgHead" False [DEItem (DEval (DEStr "HNHBK"))
                                                                    ,DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 (Just 1) Nothing)]]]))
    , testCase "One item message -- value outside of valids" $
      assertEq (Left "Value '3' for key 'MsgHead.de1' not in valid values '[\"1\",\"2\"]'")
               (fillMsg
                (M.fromList [("MsgHead.de1", DEStr "3")])
                (MSG False False [SF 0 Nothing [SEG "MsgHead" False [DEItem (DEdef "de1" AN 5 Nothing 1 (Just 1) (Just ["1","2"]))]]]))
    , testCase "Message with one DEG with two DEs" $
      assertEq (Right [[[DEStr "99", DEStr "77"],[DEStr "000000000019"]]])
               (fillMsg
                (M.fromList [("MsgHead.deg1.de1", DEStr "99"), ("MsgHead.deg1.de2", DEStr "77" )])
                (MSG False False
                 [SF 0 Nothing
                  [SEG "MsgHead" False
                   [DEGItem (DEG "deg1" 0 Nothing [DEdef "de1" AN 1 Nothing 1 (Just 1) Nothing
                                                  ,DEdef "de2" AN 2 Nothing 1 (Just 1) Nothing])
                   ,DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 (Just 1) Nothing)]]]))
    -- What else to test?
    -- Validation of minsize, maxsize, minnum, maxnum
    -- Validation of DETypes
    -- Messages with SFs
    -- Setting binary types
    -- How does signing and encryption work? (I'll only do Pin-Tan for now)
    ]
  ]

fullMsgGenTests :: [TF.Test]
fullMsgGenTests =
  [ testGroup "Test full generation of HBCI messages"
    [ testCase "DialogInitAnon" $
      let vals = M.fromList $ map (second DEStr) [("Idn.country", "0")
                                                  ,("BPD", "0")
                                                  ,("UPD", "0")
                                                  ,("lang", "0")
                                                  ,("prodName", "HsBCI")
                                                  ,("prodVersion", "0.1.0")]
      in assertEq
         (Right "HNHBK:1:3:+000000000107+220+0+1+0:1'HKIDN:2:2:+0:+9999999999+0+0'HKVVB:3:2:+0+0+0+HsBCI+0.1.0'HNHBS:4:1:+1'")
         (testF dialogInitAnon vals)
    ]
  ]
  where
    testF msg vals = fillMsg vals msg >>= return . gen

parseBankPropsLineTests :: [TF.Test]
parseBankPropsLineTests =
  [ testGroup "Test parseBankPropsLine function"
    [ testCase "BLZ too short" $
      assertEq (Left "BLZ '1234567' has wrong format")
      (parseBankPropsLine "1234567=a|b|c")
    , testCase "Not enough properties" $
      assertEq (Left "Properties have the wrong format")
      (parseBankPropsLine "12345678=a|b|c|d|e|f|g|")
    , testCase "Too many enough properties" $
      assertEq (Left "Properties have the wrong format")
      (parseBankPropsLine "12345678=a|b|c|d|e|f|g|h|i|")
    , testCase "Success" $
      assertEq (Right ("12345678",  BankProperties "Raiffeisenbank Sonnenwald"
                                                   "Auerbach, Niederbay"
                                                   "GENODEF1AUS"
                                                   "hbci01.fiducia.de"
                                                   "https://hbci11.fiducia.de/cgi-bin/hbciservlet"
                                                   "300"
                                                   "300"))
      (parseBankPropsLine "12345678=Raiffeisenbank Sonnenwald|Auerbach, Niederbay|GENODEF1AUS|88|hbci01.fiducia.de|https://hbci11.fiducia.de/cgi-bin/hbciservlet|300|300|")
    ]
  ]

validateAndExtractSegTests :: [TF.Test]
validateAndExtractSegTests =
  [ testGroup "Test validateAndExtractSeg function"
    [ testCase "SF with minnum 1 and empty MSGVal gives error" $
      assertEq (Left "Required SEG 'MsgHead' not found") $
      validateAndExtractSeg (sf0 { sfMinNum = 1}) []
    , testCase "SF with minnum 0 and empty MSGVal discarded" $
      assertEq (Right ([], [])) $
      validateAndExtractSeg sf0 []
    , testCase "Single SF with minnum 0 discarded if not matched" $
      assertEq (Right ([[[DEStr "HNHBK2"]]], [])) $
      validateAndExtractSeg sf0 [[[DEStr "HNHBK2"]]]
    , testCase "Extract some entries from a single entry" $
      assertEq (Right ([],[("MsgHead.msgsize",DEStr "000000000123"),("MsgHead.SegHead.seq",DEStr "1")])) $
      validateAndExtractSeg sf1 [[[DEStr "HNHBK", DEStr "1", DEStr "3"],[DEStr "000000000123"]]]
    ]
  ]
  where
    sf0 = SF {sfMinNum = 0,
              sfMaxNum = Just 1,
              sfItems = [SEG {segName = "MsgHead",
                              needsRequestTag = False,
                              segItems = [DEGItem (DEG "" 1 (Just 1) [(DEval (DEStr "HNHBK"))])]}]}
    sf1 = head $ msgItems $ dialogInitAnon

validateAndExtractTests :: [TF.Test]
validateAndExtractTests =
  [ testGroup "Test validateAndExtract function"
    [ testCase "Extract from generatd 'DialogInitAnon'" $
      let keys    = ["Idn.country", "BPD", "UPD", "lang", "prodName", "prodVersion"]
          vals    = ["0",  "0", "0", "0", "HsBCI", "0.1.0"]
          inputs  = M.fromList $ zip keys (map DEStr vals)
          retVals = do msg <- fillMsg inputs dialogInitAnon
                       out <- validateAndExtract dialogInitAnon msg
                       return $ M.fromList $ catMaybes [(\v -> (k,v)) <$> M.lookup k out | k <- keys]
      in assertEq (Right inputs) retVals
    ]
  ]

standaloneTests :: [TF.Test]
standaloneTests = concat [ parserTests
                         , elemToDETests
                         , elemToValidsTests
                         , elemToValueTests
                         , setDETests
                         , elemToDEGTests
                         , elemToSEGTests
                         , fillDeTests
                         , fillMsgTests
                         , elemToSFTests
                         , elemToMSGTests
                         , fullMsgGenTests
                         , parseBankPropsLineTests
                         , validateAndExtractSegTests
                         , validateAndExtractTests
                         ]

xmlTests :: [[Content] -> TF.Test]
xmlTests = getMSGfromXMLTest

main :: IO ()
main = do
  hbciPlus <- getXml "resources/hbci-plus.xml"
  defaultMain (standaloneTests ++ map ($ hbciPlus) xmlTests)

dialogInitAnon :: MSG
dialogInitAnon =
  (MSG {msgRequiresSignature = False,
                   msgRequiresEncryption = False,
                   msgItems = [SF {sfMinNum = 1,
                                   sfMaxNum = Just 1,
                                   sfItems = [SEG {segName = "MsgHead",
                                                   needsRequestTag = False,
                                                   segItems = [DEGItem (DEG {degName = "SegHead",
                                                                             degMinNum = 1,
                                                                             degMaxNum = Just 1,
                                                                             degItems = [DEval (DEStr "HNHBK")
                                                                                        ,DEdef {deName = "seq", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing}
                                                                                        ,DEval (DEStr "3")
                                                                                        ,DEdef {deName = "ref", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 0, deMaxNum = Nothing, deValids = Nothing}]})
                                                              ,DEItem (DEdef {deName = "msgsize", deType = Dig, deMinSize = 12, deMaxSize = Just 12, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing})
                                                              ,DEItem (DEval (DEStr "220"))
                                                              ,DEItem (DEval (DEStr "0"))
                                                              ,DEItem (DEval (DEStr "1"))
                                                              ,DEGItem (DEG {degName = "",
                                                                             degMinNum = 0,
                                                                             degMaxNum = Just 1,
                                                                             degItems = [DEval (DEStr "0")
                                                                                        ,DEval (DEStr "1")]})]}]}
                              ,SF {sfMinNum = 1,
                                   sfMaxNum = Just 1,
                                   sfItems = [SEG {segName = "Idn",
                                                   needsRequestTag = False,
                                                   segItems = [DEGItem (DEG {degName = "SegHead",
                                                                             degMinNum = 1,
                                                                             degMaxNum = Just 1,
                                                                             degItems = [DEval (DEStr "HKIDN")
                                                                                        ,DEdef {deName = "seq", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing}
                                                                                        ,DEval (DEStr "2")
                                                                                        ,DEdef {deName = "ref", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 0, deMaxNum = Nothing, deValids = Nothing}]})
                                                              ,DEGItem (DEG {degName = "",
                                                                             degMinNum = 1,
                                                                             degMaxNum = Just 1,
                                                                             degItems = [DEdef {deName = "country", deType = Ctr, deMinSize = 0, deMaxSize = Nothing, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing}
                                                                                        ,DEdef {deName = "blz", deType = AN, deMinSize = 0, deMaxSize = Just 30, deMinNum = 0, deMaxNum = Nothing, deValids = Nothing}]})
                                                              ,DEItem (DEval (DEStr "9999999999"))
                                                              ,DEItem (DEval (DEStr "0"))
                                                              ,DEItem (DEval (DEStr "0"))]}]}
                              ,SF {sfMinNum = 1,
                                   sfMaxNum = Just 1,
                                   sfItems = [SEG {segName = "",
                                                   needsRequestTag = False,
                                                   segItems = [DEGItem (DEG {degName = "SegHead",
                                                                             degMinNum = 1,
                                                                             degMaxNum = Just 1,
                                                                             degItems = [DEval (DEStr "HKVVB")
                                                                                        ,DEdef {deName = "seq", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing}
                                                                                        ,DEval (DEStr "2")
                                                                                        ,DEdef {deName = "ref", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 0, deMaxNum = Nothing, deValids = Nothing}]})
                                                              ,DEItem (DEdef {deName = "BPD", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing})
                                                              ,DEItem (DEdef {deName = "UPD", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing})
                                                              ,DEItem (DEdef {deName = "lang", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 1, deMaxNum = Nothing, deValids = Just ["0","1","2","3"]})
                                                              ,DEItem (DEdef {deName = "prodName", deType = AN, deMinSize = 0, deMaxSize = Just 25, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing})
                                                              ,DEItem (DEdef {deName = "prodVersion", deType = AN, deMinSize = 0, deMaxSize = Just 5, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing})]}]}
                              ,SF {sfMinNum = 1,
                                   sfMaxNum = Just 1,
                                   sfItems = [SEG {segName = "MsgTail",
                                                   needsRequestTag = False,
                                                   segItems = [DEGItem (DEG {degName = "SegHead",
                                                                             degMinNum = 1,
                                                                             degMaxNum = Just 1,
                                                                             degItems = [DEval (DEStr "HNHBS")
                                                                                        ,DEdef {deName = "seq", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 1, deMaxNum = Nothing, deValids = Nothing}
                                                                                        ,DEval (DEStr "1")
                                                                                        ,DEdef {deName = "ref", deType = Num, deMinSize = 0, deMaxSize = Just 3, deMinNum = 0, deMaxNum = Nothing, deValids = Nothing}]})
                                                              ,DEItem (DEval (DEStr "1"))]}]}]})


dialogInitAnonRes :: MSG
dialogInitAnonRes =
      (MSG False False [(SF 1 (Just 1) [(SEG "MsgHead" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNHBK")),
                                                                                                  (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                                  (DEval (DEStr "3")),
                                                                                                  (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                              (DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 Nothing Nothing)),
                                                              (DEItem (DEval (DEStr "220"))),
                                                              (DEItem (DEdef "dialogid" ID 0 Nothing 1 Nothing Nothing)),
                                                              (DEItem (DEdef "msgnum" Num 0 (Just 4) 1 Nothing Nothing)),
                                                              (DEGItem (DEG "" 0 (Just 1) [(DEdef "dialogid" ID 0 Nothing 1 Nothing Nothing),
                                                                                           (DEdef "msgnum" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 (Just 1) [(SEG "SigHead" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNSHK")),
                                                                                                  (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                                  (DEval (DEStr "3")),
                                                                                                  (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                              (DEItem (DEdef "secfunc" AN 0 (Just 3) 1 Nothing Nothing)),
                                                              (DEItem (DEdef "seccheckref" AN 0 (Just 14) 1 Nothing Nothing)),
                                                              (DEItem (DEval (DEStr "1"))),
                                                              (DEItem (DEdef "role" AN 0 (Just 3) 1 Nothing (Just ["1",
                                                                                                                   "3",
                                                                                                                   "4"]))),
                                                              (DEGItem (DEG "" 1 (Just 1) [(DEdef "func" AN 0 (Just 3) 1 Nothing (Just ["1",
                                                                                                                                        "2"])),
                                                                                           (DEdef "cid" Bin 0 (Just 256) 0 Nothing Nothing),
                                                                                           (DEdef "sysid" ID 0 Nothing 0 Nothing Nothing)])),
                                                              (DEItem (DEdef "secref" Num 0 (Just 16) 1 Nothing Nothing)),
                                                              (DEGItem (DEG "" 1 (Just 1) [(DEdef "type" AN 0 (Just 3) 1 Nothing (Just ["1",
                                                                                                                                        "6"])),
                                                                                           (DEdef "date" Date 0 Nothing 0 Nothing Nothing),
                                                                                           (DEdef "time" Time 0 Nothing 0 Nothing Nothing)])),
                                                              (DEGItem (DEG "" 1 (Just 1) [(DEval (DEStr "1")),
                                                                                           (DEdef "alg" AN 0 (Just 3) 1 Nothing (Just ["999"])),
                                                                                           (DEval (DEStr "1")),
                                                                                           (DEdef "parameter" Bin 0 (Just 512) 0 Nothing Nothing)])),
                                                              (DEGItem (DEG "" 1 (Just 1) [(DEval (DEStr "6")),
                                                                                           (DEdef "alg" AN 0 (Just 3) 1 Nothing (Just ["1",
                                                                                                                                       "10"])),
                                                                                           (DEdef "mode" AN 0 (Just 999) 1 Nothing (Just ["999",
                                                                                                                                          "16"]))])),
                                                              (DEGItem (DEG "" 1 (Just 1) [(DEdef "blz" AN 0 (Just 30) 0 Nothing Nothing),
                                                                                           (DEdef "country" Ctr 0 Nothing 1 Nothing Nothing),
                                                                                           (DEdef "userid" ID 0 Nothing 1 Nothing Nothing),
                                                                                           (DEdef "keytype" AN 0 (Just 1) 1 Nothing (Just ["S",
                                                                                                                                           "V"])),
                                                                                           (DEdef "keynum" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                           (DEdef "keyversion" Num 0 (Just 3) 1 Nothing Nothing)])),
                                                              (DEGItem (DEG "" 0 (Just 1) [(DEdef "type" Num 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                         "2",
                                                                                                                                         "3"])),
                                                                                           (DEdef "cert" Bin 0 (Just 2048) 1 Nothing Nothing)]))])]),
                        (SF 1 (Just 1) [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIRMG")),
                                                                                           (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                           (DEval (DEStr "2")),
                                                                                           (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                       (DEGItem (DEG "" 1 (Just 99) [(DEdef "code" Dig 4 (Just 4) 1 Nothing Nothing),
                                                                                     (DEdef "ref" AN 0 (Just 7) 0 Nothing Nothing),
                                                                                     (DEdef "text" AN 0 (Just 80) 1 Nothing Nothing),
                                                                                     (DEdef "parm" AN 0 (Just 35) 0 (Just 10) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIRMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEGItem (DEG "" 1 (Just 99) [(DEdef "code" Dig 4 (Just 4) 1 Nothing Nothing),
                                                                                    (DEdef "ref" AN 0 (Just 7) 0 Nothing Nothing),
                                                                                    (DEdef "text" AN 0 (Just 80) 1 Nothing Nothing),
                                                                                    (DEdef "parm" AN 0 (Just 35) 0 (Just 10) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEdef "code" AN 0 (Just 6) 1 Nothing Nothing),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEdef "version" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEdef "code" AN 0 (Just 6) 1 Nothing Nothing),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEdef "version" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclassdummy" AN 0 (Just 999) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEdef "code" AN 0 (Just 6) 1 Nothing Nothing),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEdef "version" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "dummy" AN 0 (Just 999) 2 (Just 999) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEdef "code" AN 0 (Just 6) 1 Nothing Nothing),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEdef "version" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "dummy" AN 0 (Just 999) 1 (Just 999) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWSDS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "needdepot" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "risikodescr" AN 0 (Just 38) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWSDS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "needdepot" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "risikodescr" AN 0 (Just 38) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWSDS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPRS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "searchallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "regionallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "standardallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "newallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "availboerses" AN 0 (Just 4096) 0 Nothing Nothing),
                                                                                   (DEdef "availtypes" AN 0 (Just 35) 0 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPRS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "searchallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "regionallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "standardallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "newallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "availboerses" AN 0 (Just 4096) 0 Nothing Nothing),
                                                                                   (DEdef "availtypes" AN 0 (Just 35) 0 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPRS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "searchallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "regionallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "standardallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "newallowed" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "availboerses" AN 0 (Just 4096) 0 Nothing Nothing),
                                                                                   (DEdef "availtypes" AN 0 (Just 35) 0 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPKS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParWPKursList" 1 (Just 1) [(DEdef "needdepot" JN 0 Nothing 1 Nothing Nothing),
                                                                                                (DEdef "canquality" JN 0 Nothing 1 Nothing Nothing),
                                                                                                (DEdef "availboerses" AN 0 (Just 4096) 0 Nothing Nothing),
                                                                                                (DEdef "kurspaket" AN 0 (Just 30) 0 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPKS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParWPKursList" 1 (Just 1) [(DEdef "needdepot" JN 0 Nothing 1 Nothing Nothing),
                                                                                                (DEdef "canquality" JN 0 Nothing 1 Nothing Nothing),
                                                                                                (DEdef "availboerses" AN 0 (Just 4096) 0 Nothing Nothing),
                                                                                                (DEdef "kurspaket" AN 0 (Just 30) 0 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPKS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParWPKursList" 1 (Just 1) [(DEdef "availboerses" AN 0 (Just 2048) 0 Nothing Nothing),
                                                                                                (DEdef "kurspaket" AN 0 (Just 30) 0 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPIS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "needdepot" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPIS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "needdepot" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPIS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWDUS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWDUS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWDUS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWDUS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWDUS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPDS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "6")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParWPDepotList" 1 (Just 1) [(DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                                 (DEdef "cancurr" JN 0 Nothing 1 Nothing Nothing),
                                                                                                 (DEdef "canquality" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPDS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParWPDepotList" 1 (Just 1) [(DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                                 (DEdef "cancurr" JN 0 Nothing 1 Nothing Nothing),
                                                                                                 (DEdef "canquality" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPDS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParWPDepotList" 1 (Just 1) [(DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                                 (DEdef "cancurr" JN 0 Nothing 1 Nothing Nothing),
                                                                                                 (DEdef "canquality" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPDS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParWPDepotList" 1 (Just 1) [(DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIWPDS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParWPDepotList" 1 (Just 1) [(DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIVMKS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "canallaccounts" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIUMBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIUMBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HICCSS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIGUBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIAOMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "caniban" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "countryinfo" AN 0 (Just 99) 0 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIAOMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "caniban" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "countryinfo" AN 0 (Just 99) 0 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIEILS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIUEBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIUEBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIUEBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIUEBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HICSBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "cantimerange" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITUBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "cantimerange" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITUBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "cantimerange" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITUBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "cantimerange" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITUAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITUAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITUAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITULS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITULS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITULS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HICSES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITUES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITUES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITUES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITSBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "cantimerange" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITSLS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITSES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISLBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "cantimerange" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISLLS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISLES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITABS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITABS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITABS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITABS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITAZS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParTAN2Step" 1 (Just 1) [(DEdef "can1step" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canmultitangvs" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "orderhashmode" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                                                      "1",
                                                                                                                                                      "2"])),
                                                                                              (DEdef "nofactivetanmedia" Num 0 (Just 1) 0 Nothing Nothing),
                                                                                              (DEdef "needtanmedia" Code 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "initmode" Code 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "ischallengestructured" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needchallengeklass" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needorderaccount" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "needsmsaccount" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "canstorno" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needtanlistidx" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "cantandelay" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "canmultitan" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "noftanlists" Num 0 (Just 1) 0 Nothing Nothing),
                                                                                              (DEdef "maxleninput2step" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                              (DEdef "inputinfo" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "tanformat" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                  "2"])),
                                                                                              (DEdef "maxlentan2step" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                              (DEdef "name" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "zkamethod_version" AN 0 (Just 10) 0 Nothing Nothing),
                                                                                              (DEdef "zkamethod_name" AN 0 (Just 32) 0 Nothing Nothing),
                                                                                              (DEdef "id" ID 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "process" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                "2"])),
                                                                                              (DEdef "secfunc" Code 0 (Just 3) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParTAN2Step" 1 (Just 1) [(DEdef "can1step" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canmultitangvs" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "orderhashmode" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                                                      "1",
                                                                                                                                                      "2"])),
                                                                                              (DEdef "nofactivetanmedia" Num 0 (Just 1) 0 Nothing Nothing),
                                                                                              (DEdef "needtanmedia" Code 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "initmode" Code 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "ischallengestructured" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needchallengevalue" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needchallengeklass" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needsmsaccount" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canstorno" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needtanlistidx" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "cantandelay" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "canmultitan" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "noftanlists" Num 0 (Just 1) 0 Nothing Nothing),
                                                                                              (DEdef "maxleninput2step" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                              (DEdef "inputinfo" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "tanformat" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                  "2"])),
                                                                                              (DEdef "maxlentan2step" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                              (DEdef "name" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "zkamethod_version" AN 0 (Just 10) 0 Nothing Nothing),
                                                                                              (DEdef "zkamethod_name" AN 0 (Just 32) 0 Nothing Nothing),
                                                                                              (DEdef "id" ID 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "process" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                "2"])),
                                                                                              (DEdef "secfunc" Code 0 (Just 3) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParTAN2Step" 1 (Just 1) [(DEdef "can1step" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canmultitangvs" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "orderhashmode" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                                                      "1",
                                                                                                                                                      "2"])),
                                                                                              (DEdef "nofactivetanmedia" Num 0 (Just 1) 0 Nothing Nothing),
                                                                                              (DEdef "needtanmedia" Code 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "initmode" Code 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needchallengevalue" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needchallengeklass" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canstorno" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needtanlistidx" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "cantandelay" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "canmultitan" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "noftanlists" Num 0 (Just 1) 0 Nothing Nothing),
                                                                                              (DEdef "maxleninput2step" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                              (DEdef "inputinfo" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "tanformat" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                  "2"])),
                                                                                              (DEdef "maxlentan2step" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                              (DEdef "name" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "id" ID 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "process" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                "2"])),
                                                                                              (DEdef "secfunc" Code 0 (Just 3) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParTAN2Step" 1 (Just 1) [(DEdef "can1step" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canmultitangvs" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "orderhashmode" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                                                      "1",
                                                                                                                                                      "2"])),
                                                                                              (DEdef "needchallengevalue" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needchallengeklass" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canstorno" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "needtanlistidx" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "cantandelay" Code 0 (Just 1) 1 Nothing Nothing),
                                                                                              (DEdef "canmultitan" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "noftanlists" Num 0 (Just 1) 0 Nothing Nothing),
                                                                                              (DEdef "maxleninput2step" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                              (DEdef "inputinfo" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "tanformat" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                  "2"])),
                                                                                              (DEdef "maxlentan2step" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                              (DEdef "name" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "id" ID 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "process" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                "2"])),
                                                                                              (DEdef "secfunc" Code 0 (Just 3) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HITANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParTAN2Step" 1 (Just 1) [(DEdef "can1step" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canmultitangvs" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "orderhashmode" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                                                      "1",
                                                                                                                                                      "2"])),
                                                                                              (DEdef "instsig" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                                                "1",
                                                                                                                                                "2"])),
                                                                                              (DEdef "cantandelay" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canmultitan" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "noftanlists" Num 0 (Just 1) 0 Nothing Nothing),
                                                                                              (DEdef "maxleninput2step" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                              (DEdef "inputinfo" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "tanformat" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                  "2"])),
                                                                                              (DEdef "maxlentan2step" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                              (DEdef "name" AN 0 (Just 30) 1 Nothing Nothing),
                                                                                              (DEdef "id" ID 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "process" Code 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                                "2"])),
                                                                                              (DEdef "secfunc" Code 0 (Just 3) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIPROS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIPROS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIPROS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISPAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "cansingleaccquery" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "cannationalacc" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "canstructusage" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "suppformats" AN 0 (Just 256) 0 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIBMES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "minVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxnum" Num 0 (Just 7) 1 Nothing Nothing),
                                                                                   (DEdef "needtotal" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "cansingletransfer" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDMCS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxnum" Num 0 (Just 7) 1 Nothing Nothing),
                                                                                   (DEdef "needtotal" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "cansingletransfer" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "minVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "minVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "PurposeCodes" AN 0 (Just 4096) 0 (Just 1) Nothing),
                                                                                   (DEdef "suppformats" AN 0 (Just 256) 0 (Just 9) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDMES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "minVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxnum" Num 0 (Just 7) 1 Nothing Nothing),
                                                                                   (DEdef "needtotal" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "cansingletransfer" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HICCMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxnum" Num 0 (Just 7) 1 Nothing Nothing),
                                                                                   (DEdef "needtotal" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "cansingletransfer" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDTES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISUBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "6")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISUBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISUBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISUBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISLAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "6")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISLAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISLAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISLAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxcsets" Num 0 (Just 6) 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISALS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "7")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISALS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "6")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISALS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISALS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISALS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIPINS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParPinTan" 1 (Just 1) [(DEdef "pinlen_min" Num 0 (Just 2) 0 Nothing Nothing),
                                                                                            (DEdef "pinlen_max" Num 0 (Just 2) 0 Nothing Nothing),
                                                                                            (DEdef "tanlen_max" Num 0 (Just 2) 0 Nothing Nothing),
                                                                                            (DEdef "info_userid" AN 0 (Just 30) 0 Nothing Nothing),
                                                                                            (DEdef "info_customerid" AN 0 (Just 30) 0 Nothing Nothing),
                                                                                            (DEdef "needtan" JN 0 Nothing 1 Nothing Nothing),
                                                                                            (DEdef "segcode" AN 0 (Just 6) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "DIPINS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "needtan" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "segcode" AN 0 (Just 6) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIAUES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HILSWS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "curr" Cur 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "value" Wrt 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HILSWS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "curr" Cur 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "value" Wrt 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIBSES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParTermSepaB2B" 1 (Just 1) [(DEdef "minVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                 (DEdef "maxVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                 (DEdef "minVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                 (DEdef "maxVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDSCS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParTermSepaCOR1" 1 (Just 1) [(DEdef "minVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                  (DEdef "maxVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                  (DEdef "minVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                  (DEdef "maxVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                  (DEdef "PurposeCodes" AN 0 (Just 4096) 1 Nothing Nothing),
                                                                                                  (DEdef "suppformats" AN 0 (Just 256) 0 (Just 9) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDSES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParTermSepaEinzelLast" 1 (Just 1) [(DEdef "minVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                        (DEdef "maxVorlZeitFNALRCUR" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                        (DEdef "minVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                        (DEdef "maxVorlZeitFRSTOOFF" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HILASS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HILASS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HILASS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HILASS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "key" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKAZS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "7")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParKUmsZeitSEPA" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                  (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                                  (DEdef "canallaccounts" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKAZS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "6")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParKUmsZeit" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                              (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canallaccounts" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKAZS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParKUmsZeit" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                              (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                              (DEdef "canallaccounts" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKAZS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParKUmsZeit" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                              (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "7")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParKUmsNewSEPA" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                                 (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                                 (DEdef "canallaccounts" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "6")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParKUmsNew" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                             (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "canallaccounts" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParKUmsNew" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                             (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "canallaccounts" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParKUmsNew" 1 (Just 1) [(DEdef "timerange" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                             (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIEKAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "canindex" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "needreceipt" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "format" Code 0 (Just 1) 1 (Just 9) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIEKAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "canindex" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "needreceipt" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "format" Code 0 (Just 1) 1 (Just 9) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIEKAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "canindex" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "needreceipt" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "format" Code 0 (Just 1) 1 (Just 9) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIEKAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "canindex" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "needreceipt" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "canmaxentries" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "format" Code 0 (Just 1) 1 (Just 9) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKIAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKIAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKIAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKIAS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIINFS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIINFS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIINFS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIINFS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIFGNS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "ParFestNew" 1 (Just 1) [(DEdef "canexistinganlkto" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "canotherausbuchungskto" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "canotherzinskto" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "kontoauszug" Code 0 (Just 1) 0 (Just 9) (Just ["1",
                                                                                                                                                    "2"]))]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIFGNS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParFestNew" 1 (Just 1) [(DEdef "canexistinganlkto" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "canotherausbuchungskto" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "canotherzinskto" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "kontoauszug" Code 0 (Just 1) 0 (Just 9) (Just ["1",
                                                                                                                                                    "2"]))]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIFGNS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "ParFestNew" 1 (Just 1) [(DEdef "canotherausbuchungskto" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "canotherzinskto" JN 0 Nothing 1 Nothing Nothing),
                                                                                             (DEdef "kontoauszug" Code 0 (Just 1) 0 (Just 9) (Just ["1",
                                                                                                                                                    "2"]))]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIFGBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIFGBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIFGBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIFGKS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "curr" Cur 0 Nothing 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIFGKS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "curr" Cur 0 Nothing 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIFGKS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "curr" Cur 0 Nothing 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HICDES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDAES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing),
                                                                                   (DEdef "textkey" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDAES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing),
                                                                                   (DEdef "textkey" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDAES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing),
                                                                                   (DEdef "textkey" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDAES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing),
                                                                                   (DEdef "textkey" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HICDBS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEItem (DEdef "maxentries_allowed" JN 0 Nothing 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDABS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDABS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDABS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDABS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDABS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HICDNS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "numtermchanges" Num 0 (Just 1) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "recktoeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "recnameeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "valueeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "usageeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "firstexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "timeuniteditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "turnuseditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "execdayeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "lastexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "numtermchanges" Num 0 (Just 1) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "recktoeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "recnameeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "valueeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "keyeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "usageeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "firstexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "timeuniteditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "turnuseditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "execdayeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "lastexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing),
                                                                                   (DEdef "textkey" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "numtermchanges" Num 0 (Just 1) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "recktoeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "recnameeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "valueeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "keyeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "usageeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "firstexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "timeuniteditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "turnuseditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "execdayeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "lastexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing),
                                                                                   (DEdef "textkey" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "numtermchanges" Num 0 (Just 1) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "recktoeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "recnameeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "valueeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "keyeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "usageeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "firstexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "timeuniteditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "turnuseditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "execdayeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "lastexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing),
                                                                                   (DEdef "textkey" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDANS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "numtermchanges" Num 0 (Just 1) 1 Nothing Nothing),
                                                                                   (DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "recktoeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "recnameeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "valueeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "keyeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "usageeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "firstexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "timeuniteditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "turnuseditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "execdayeditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "lastexeceditable" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "maxusage" Num 0 (Just 2) 1 Nothing Nothing),
                                                                                   (DEdef "turnusmonths" Dig 0 (Just 24) 1 Nothing Nothing),
                                                                                   (DEdef "dayspermonth" Dig 0 (Just 64) 1 Nothing Nothing),
                                                                                   (DEdef "turnusweeks" Dig 0 (Just 104) 0 Nothing Nothing),
                                                                                   (DEdef "daysperweek" Dig 0 (Just 7) 0 Nothing Nothing),
                                                                                   (DEdef "textkey" Dig 2 (Just 2) 1 (Just 99) Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HICDLS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "cantermdel" JN 0 Nothing 1 Nothing Nothing),
                                                                                   (DEdef "orderdata_required" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDALS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "cantermdel" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDALS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "cantermdel" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDALS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "cantermdel" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIDALS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "minpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "maxpretime" Num 0 (Just 4) 1 Nothing Nothing),
                                                                                   (DEdef "cantermdel" JN 0 Nothing 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKDMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "5")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"]))),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxlen" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKDMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxlen" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKDMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxlen" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKDMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEGItem (DEG "" 1 (Just 1) [(DEdef "maxlen" Num 0 (Just 4) 1 Nothing Nothing)]))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKOMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "4")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKOMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "3")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKOMS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "DIPAES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIPAES")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIAZKS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIAZKS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKIFS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "secclass" Code 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1",
                                                                                                                 "2",
                                                                                                                 "3",
                                                                                                                 "4"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKIFS")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "1")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "maxnum" Num 0 (Just 3) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "minsigs" Num 0 (Just 1) 1 Nothing Nothing))])]),
                        (SF 0 (Just 1) [(SEG "SecMethod" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKPV")),
                                                                                                    (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                                    (DEval (DEStr "1")),
                                                                                                    (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                                (DEGItem (DEG "" 1 (Just 9) [(DEval (DEStr "0")),
                                                                                             (DEval (DEStr "0"))]))])]),
                        (SF 0 (Just 1) [(SEG "SecMethod" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HISHV")),
                                                                                                    (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                                    (DEval (DEStr "2")),
                                                                                                    (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                                (DEItem (DEdef "mixing" JN 0 Nothing 1 Nothing Nothing)),
                                                                (DEGItem (DEG "" 1 (Just 9) [(DEdef "method" AN 3 (Just 3) 1 Nothing Nothing),
                                                                                             (DEdef "version" Num 0 (Just 3) 1 (Just 9) Nothing)]))])]),
                        (SF 0 (Just 1) [(SEG "CommListRes" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKOM")),
                                                                                                      (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                                      (DEval (DEStr "3")),
                                                                                                      (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                                  (DEGItem (DEG "" 1 (Just 1) [(DEdef "country" Ctr 0 Nothing 1 Nothing Nothing),
                                                                                               (DEdef "blz" AN 0 (Just 30) 0 Nothing Nothing)])),
                                                                  (DEItem (DEdef "deflang" Num 0 (Just 3) 1 Nothing (Just ["1",
                                                                                                                           "2",
                                                                                                                           "3"]))),
                                                                  (DEGItem (DEG "" 1 (Just 9) [(DEdef "dienst" AN 0 (Just 2) 1 Nothing (Just ["1",
                                                                                                                                              "2",
                                                                                                                                              "3"])),
                                                                                               (DEdef "addr" AN 0 (Just 512) 1 Nothing Nothing),
                                                                                               (DEdef "addr2" AN 0 (Just 512) 0 Nothing Nothing),
                                                                                               (DEdef "filter" AN 3 (Just 3) 0 Nothing (Just ["MIM",
                                                                                                                                              "UUE"])),
                                                                                               (DEdef "filterversion" Num 0 (Just 3) 0 Nothing Nothing)]))])]),
                        (SF 0 (Just 1) [(SEG "CommListRes" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKOM")),
                                                                                                      (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                                      (DEval (DEStr "2")),
                                                                                                      (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                                  (DEGItem (DEG "" 1 (Just 1) [(DEdef "country" Ctr 0 Nothing 1 Nothing Nothing),
                                                                                               (DEdef "blz" AN 0 (Just 30) 0 Nothing Nothing)])),
                                                                  (DEItem (DEdef "deflang" Num 0 (Just 3) 1 Nothing (Just ["1",
                                                                                                                           "2",
                                                                                                                           "3"]))),
                                                                  (DEGItem (DEG "" 1 (Just 9) [(DEdef "dienst" AN 0 (Just 2) 1 Nothing (Just ["1",
                                                                                                                                              "2",
                                                                                                                                              "3"])),
                                                                                               (DEdef "addr" AN 0 (Just 512) 1 Nothing Nothing),
                                                                                               (DEdef "addr2" AN 0 (Just 512) 0 Nothing Nothing),
                                                                                               (DEdef "filter" AN 3 (Just 3) 0 Nothing (Just ["MIM",
                                                                                                                                              "UUE"])),
                                                                                               (DEdef "filterversion" Num 0 (Just 3) 0 Nothing Nothing)]))])]),
                        (SF 0 (Just 1) [(SEG "BPA" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIBPA")),
                                                                                              (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                              (DEval (DEStr "2")),
                                                                                              (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                          (DEItem (DEdef "version" Num 0 (Just 3) 1 Nothing Nothing)),
                                                          (DEGItem (DEG "" 1 (Just 1) [(DEdef "country" Ctr 0 Nothing 1 Nothing Nothing),
                                                                                       (DEdef "blz" AN 0 (Just 30) 0 Nothing Nothing)])),
                                                          (DEItem (DEdef "kiname" AN 0 (Just 60) 1 Nothing Nothing)),
                                                          (DEItem (DEdef "numgva" Num 0 (Just 3) 1 Nothing Nothing)),
                                                          (DEGItem (DEG "" 1 (Just 1) [(DEdef "lang" Num 0 (Just 3) 1 (Just 9) (Just ["1",
                                                                                                                                      "2",
                                                                                                                                      "3"]))])),
                                                          (DEGItem (DEG "" 1 (Just 1) [(DEdef "version" Num 0 (Just 3) 1 (Just 9) (Just ["201",
                                                                                                                                         "210",
                                                                                                                                         "220",
                                                                                                                                         "300",
                                                                                                                                         "400"]))])),
                                                          (DEItem (DEdef "maxmsgsize" Num 0 (Just 4) 0 Nothing Nothing))])]),
                        (SF 0 Nothing [(SEG "KInfo" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIUPD")),
                                                                                               (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                               (DEval (DEStr "4")),
                                                                                               (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                           (DEGItem (DEG "KTV" 1 (Just 1) [(DEdef "number" ID 0 Nothing 1 Nothing Nothing),
                                                                                           (DEdef "subnumber" ID 0 Nothing 0 Nothing Nothing),
                                                                                           (DEdef "KIK.country" Ctr 0 Nothing 1 Nothing Nothing),
                                                                                           (DEdef "KIK.blz" AN 0 (Just 30) 0 Nothing Nothing)])),
                                                           (DEItem (DEdef "customerid" ID 0 Nothing 1 Nothing Nothing)),
                                                           (DEItem (DEdef "cur" Cur 0 Nothing 0 Nothing Nothing)),
                                                           (DEItem (DEdef "name1" AN 0 (Just 27) 1 Nothing Nothing)),
                                                           (DEItem (DEdef "name2" AN 0 (Just 27) 0 Nothing Nothing)),
                                                           (DEItem (DEdef "konto" AN 0 (Just 30) 0 Nothing Nothing)),
                                                           (DEGItem (DEG "" 0 (Just 1) [(DEdef "limittype" AN 0 (Just 1) 1 Nothing (Just ["E", "T", "W", "M", "Z"])),
                                                                                        (DEdef "curr" Cur 0 Nothing 1 Nothing Nothing),
                                                                                        (DEdef "value" Wrt 0 Nothing 1 Nothing Nothing),
                                                                                        (DEdef "limitdays" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                           (DEGItem (DEG "" 0 (Just 999) [(DEdef "code" AN 0 (Just 6) 1 Nothing Nothing),
                                                                                          (DEdef "reqSigs" Num 0 (Just 2) 1 Nothing (Just ["0", "1", "2", "3", "98", "99"])),
                                                                                          (DEdef "limittype" AN 0 (Just 1) 0 Nothing (Just ["E", "T", "W", "M", "Z"])),
                                                                                          (DEdef "value" Wrt 0 Nothing 0 Nothing Nothing),
                                                                                          (DEdef "curr" Cur 0 Nothing 0 Nothing Nothing),
                                                                                          (DEdef "limitdays" Num 0 (Just 3) 0 Nothing Nothing)]))])]),
                        (SF 0 (Just 1) [(SEG "UPA" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIUPA")),
                                                                                              (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                              (DEval (DEStr "2")),
                                                                                              (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                          (DEItem (DEdef "userid" ID 0 Nothing 1 Nothing Nothing)),
                                                          (DEItem (DEdef "version" Num 0 (Just 3) 1 Nothing Nothing)),
                                                          (DEItem (DEdef "usage" Num 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                                 "1"])))])]),
                        (SF 0 Nothing [(SEG "" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HIKIM")),
                                                                                          (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                          (DEval (DEStr "2")),
                                                                                          (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                      (DEItem (DEdef "betreff" AN 0 (Just 35) 1 Nothing Nothing)),
                                                      (DEItem (DEdef "text" AN 0 (Just 2048) 1 Nothing Nothing))])]),
                        (SF 0 (Just 1) [(SEG "SigTail" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNSHA")),
                                                                                                  (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                                  (DEval (DEStr "1")),
                                                                                                  (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                              (DEItem (DEdef "seccheckref" AN 0 (Just 14) 1 Nothing Nothing)),
                                                              (DEItem (DEdef "sig" Bin 0 (Just 512) 0 Nothing Nothing)),
                                                              (DEGItem (DEG "" 0 (Just 1) [(DEdef "pin" AN 0 (Just 99) 1 Nothing Nothing),
                                                                                           (DEdef "tan" AN 0 (Just 35) 0 Nothing Nothing)]))])]),
                        (SF 1 (Just 1) [(SEG "MsgTail" False [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNHBS")),
                                                                                                  (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                                  (DEval (DEStr "1")),
                                                                                                  (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                              (DEItem (DEdef "msgnum" Num 0 (Just 4) 1 Nothing Nothing))])])])
