{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Applicative ((<$>))
import           Control.Monad.State (evalStateT, runStateT)
import           Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.IntMap as IM
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Text.XML.Light (parseXML, onlyElems, Content(..))

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
    , testCase "KIK" $
      assertEq (Right ("KIK",
                       DEG "" 0 Nothing
                       [ DEdef "country" Ctr 0 Nothing   1 Nothing Nothing
                       , DEdef "blz"     AN  0 (Just 30) 0 Nothing Nothing
                       ]))
      (testF "<DEGdef id=\"KIK\"> <DE name=\"country\" type=\"Ctr\" /> <DE maxsize=\"30\" minnum=\"0\" name=\"blz\" type=\"AN\" /> </DEGdef>")
    ]
  , testGroup "Known examples of DEGdefs with DEGs inside of them"
    [ testCase "DEGdef with DEGs" $
      assertEq (Right ("KeyName",
                       DEG "" 0 Nothing
                       [ DEdef "country" Ctr 0 Nothing   1 Nothing Nothing
                       , DEdef "blz"     AN  0 (Just 30) 0 Nothing Nothing
                       ]))
      (let knownDEGs = M.fromList [("KIK", DEG "" 0 Nothing [ DEdef "country" Ctr 0 Nothing   1 Nothing Nothing
                                                            , DEdef "blz"     AN  0 (Just 30) 0 Nothing Nothing
                                                            ])]
       in testF2 knownDEGs "<DEGdef id=\"KeyName\"><DEG type=\"KIK\" /></DEGdef>")
    ]
    -- FIXME: Test failures
  ]
  where
    testF :: BS.ByteString -> Either T.Text (T.Text, DEG)
    testF = testF2 M.empty

    testF2 :: M.Map T.Text DEG -> BS.ByteString -> Either T.Text (T.Text, DEG)
    testF2 degs = elemToDEG degs . head . onlyElems . parseXML

elemToSEGTests :: [TF.Test]
elemToSEGTests =
  [ testGroup "Constructed SEGdefs examples"
    [ testCase "Empty SEGdef" $
      assertEq (Right ("01", SEG "" False 1 (Just 1) []))
      (testF1 "<SEGdef id=\"01\"></SEGdef>")
    , testCase "SEGdef with single DE" $
      assertEq (Right ("01", SEG "" False 1 (Just 1) [DEItem (DEdef "de01" AN 0 Nothing 1 Nothing Nothing)]))
      (testF1 "<SEGdef id=\"01\"><DE name=\"de01\" type=\"AN\"/></SEGdef>")
    , testCase "SEGdef with single DEG" $
      assertEq (Right ("01", SEG "" False 1 (Just 1) [DEGItem (DEG "DegName" 0 (Just 2) [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])]))
      (testF2 (M.fromList [("deg01", DEG "" 0 Nothing [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])])
       "<SEGdef id=\"01\"><DEG type=\"deg01\" name=\"DegName\" minnum=\"0\" maxnum=\"2\"/></SEGdef>")
    , testCase "SEGdef with single DEG and default attributes" $
      assertEq (Right ("01", SEG "" False 1 (Just 1) [DEGItem (DEG "deg01" 1 (Just 1) [])]))
      (testF2 (M.fromList [("deg01", DEG "" 0 Nothing [])]) "<SEGdef id=\"01\"><DEG type=\"deg01\"/></SEGdef>")
    , testCase "SEGdef with single DEG and value" $
      assertEq (Right ("01", SEG "" False 1 (Just 1) [DEGItem (DEG "DegName" 0 (Just 2) [DEval (DEStr "abcdefgh")])]))
      (testF2 (M.fromList [("deg01", DEG "DegName" 0 Nothing [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])])
       "<SEGdef id=\"01\"><DEG type=\"deg01\" name=\"DegName\" minnum=\"0\" maxnum=\"2\"/><value path=\"DegName.de01\">abcdefgh</value></SEGdef>")
    , testCase "SEGdef with single DEG and valids" $
      assertEq (Right ("01", SEG "" False 1 (Just 1) [DEGItem (DEG "DegName" 0 (Just 2) [DEdef "de01" AN 0 Nothing 1 Nothing (Just ["a","b","c"])])]))
      (testF2 (M.fromList [("deg01", DEG "DegName" 0 Nothing [DEdef "de01" AN 0 Nothing 1 Nothing Nothing])])
       "<SEGdef id=\"01\"><DEG type=\"deg01\" name=\"DegName\" minnum=\"0\" maxnum=\"2\"/><valids path=\"DegName.de01\"><validvalue>a</validvalue><validvalue>b</validvalue><validvalue>c</validvalue></valids></SEGdef>")
    , testCase "SEGdef with DEs and DEGs and values and valids" $
      assertEq
      (Right ("Seg01",
             SEG "" False 1 (Just 1)
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
      assertEq (Right ("sf01", [SEG "segName01" False 5 (Just 7) []]))
      (testF2 (M.fromList [("segId01", SEG "" False 0 Nothing [])]) M.empty
       "<SFdef id=\"sf01\"><SEG type=\"segId01\" name=\"segName01\" minnum=\"5\" maxnum=\"7\"/></SFdef>")
    , testCase "Two Segs in SF" $
      assertEq (Right ("sf01", [SEG "segName01" False 5 (Just 7) [], SEG "segName02" False 1 (Just 1) []]))
      (testF2 (M.fromList [("segId01", SEG "" False 0 Nothing []),("segId02", SEG "" False 0 Nothing [])]) M.empty
       "<SFdef id=\"sf01\"><SEG type=\"segId01\" name=\"segName01\" minnum=\"5\" maxnum=\"7\"/><SEG type=\"segId02\" name=\"segName02\"/></SFdef>")
    , testCase "Single SF in SFdef" $
      assertEq (Right ("sf01", [SEG "seg01" False 0 Nothing [], SEG "seg02" False 0 Nothing []]))
      (testF2 M.empty (M.fromList [("sfId01", [SEG "seg01" False 0 Nothing [], SEG "seg02" False 0 Nothing []])])
       "<SFdef id=\"sf01\"><SF type=\"sfId01\"/></SFdef>")
    ]
  ]
  where
    testF :: BS.ByteString -> Either T.Text (T.Text, [SEG])
    testF = elemToSF M.empty M.empty . head . onlyElems . parseXML

    testF2 :: M.Map T.Text SEG -> M.Map T.Text [SEG] -> BS.ByteString -> Either T.Text (T.Text, [SEG])
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
    , testCase "MSGdef with single SF and default values" $
      assertEq (Right ("msg01", MSG True True []))
      (testF M.empty (M.fromList [("sf01", [])]) "<MSGdef id=\"msg01\"><SF type=\"sf01\"/></MSGdef>")
    , testCase "MSGdef with single SEG which doesn't exit in the dicts" $
      assertEq (Left "1: SEG: Referenced element 'seg01' not found")
      (testF M.empty M.empty "<MSGdef id=\"msg01\"><SEG type=\"seg01\"/></MSGdef>")
    , testCase "MSGdef with single SEG, minnum, maxnum unspecified" $
      assertEq (Right ("msg01", MSG True True [SEG "seg01" False 1 (Just 1) []]))
      (testF (M.fromList [("seg01", SEG "" False 1 (Just 1) [])]) M.empty "<MSGdef id=\"msg01\"><SEG type=\"seg01\"/></MSGdef>")
    , testCase "MSGdef with single SEG, name, minnum, and maxnum" $
      assertEq (Right ("msg01", MSG True True [SEG "SegName01" False 0 (Just 99) []]))
      (testF (M.fromList [("seg01", SEG "" False 1 (Just 1) [])]) M.empty "<MSGdef id=\"msg01\"><SEG name=\"SegName01\" minnum=\"0\" maxnum=\"99\" type=\"seg01\"/></MSGdef>")
    , testCase "MSGdef with SF containing two SEGs and a SEG" $
      let segMap = M.fromList [("seg01", SEG "" False 1 (Just 1) [])]
          sfMap = M.fromList [("sf01", [])]
      in assertEq (Right ("msg01", MSG True True [SEG "SegName01" False 0 (Just 99) []]))
         (testF segMap sfMap
          ("<MSGdef id=\"msg01\">" <>
           "<SF type=\"sf01\" minnum=\"3\" maxnum=\"0\"/>" <>
           "<SEG name=\"SegName01\" minnum=\"0\" maxnum=\"99\" type=\"seg01\"/>" <>
           "</MSGdef>"))
    , testCase "MSGdef with SF containing two SEGs and a SEG and values" $
      let segMap = M.fromList [("seg02", SEG "" False 1 (Just 1) [DEItem (DEdef "DeInSeg" AN 0 Nothing 0 Nothing Nothing)])]
          sfMap = M.fromList [("sf01", [SEG "SegInSf" False 1 (Just 20) [DEGItem (DEG "DegInSf" 0 Nothing [DEdef "DeInSf" AN 0 Nothing 0 Nothing Nothing])]])]
      in assertEq (Right ("msg01", MSG True True [SEG "SegInSf" False 1 Nothing
                                                   [DEGItem (DEG "DegInSf" 0 Nothing [DEval (DEStr "123")])]
                                                ,SEG "SegOnTop" False 0 (Just 99) [DEItem (DEval (DEStr "456"))]
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
    testF :: M.Map T.Text SEG -> M.Map T.Text [SEG] -> BS.ByteString -> Either T.Text (T.Text, MSG)
    testF segs sfs = elemToMSG segs sfs . head . onlyElems . parseXML


getMSGfromXMLTest :: [[Content] -> TF.Test]
getMSGfromXMLTest =
  [ \xml -> testGroup "getMSGfromXMLTest"
    [ testCase "DialogInitAnon" $
      let res = M.lookup "DialogInitAnon" <$> getMSGfromXML xml
      in case res of
        (Right (Just res')) -> assertEqPretty dialogInitAnon res'
        _                   -> assertEq (Right (Just dialogInitAnon)) res
    , testCase "DialogInit" $
      let res = M.lookup "DialogInit" <$> getMSGfromXML xml
      in case res of
        (Right (Just res')) -> assertEqPretty dialogInit res'
        _                   -> assertEq (Right (Just dialogInit)) res
    ]
  ]

fillDeTests :: [TF.Test]
fillDeTests =
  [ testGroup "fillDeTests - simple DE examples"
    [ testCase "DEStr is not modified" $
      assertEq (Right (DEStr "abc"))
      (testF Nothing (DEval (DEStr "abc")))
    , testCase "DEBinary is not modified" $
      assertEq (Right (DEBinary "abc"))
      (testF Nothing (DEval (DEBinary "abc")))
    , testCase "Simple replacement" $
      assertEq (Right (DEStr "abc"))
      (testF (Just (DEStr "abc")) (DEdef "deKey" AN 0 Nothing 0 Nothing Nothing))
    , testCase "Replacement with prefix" $
      assertEq (Right (DEStr "abc"))
      (testF (Just (DEStr "abc")) (DEdef "deKey" AN 0 Nothing 0 Nothing Nothing))
    , testCase "Names with multiple dots are found" $
      assertEq (Right (DEStr "abc"))
      (testF (Just (DEStr "abc")) (DEdef "deKey0.deKey1" AN 0 Nothing 0 Nothing Nothing))
    , testCase "fillDe gives error if name is outside of valids" $
      assertEq (Left $ FillError ["deKey"] "Value 'abc' not in valid values '[\"ab\",\"c\",\"ac\"]'")
      (testF (Just (DEStr "abc")) (DEdef "deKey" AN 0 Nothing 0 Nothing (Just ["ab", "c", "ac"])))
    , testCase "AN replacement results in string" $
      assertEq (Right (DEStr "abc0123"))
      (testF (Just (DEStr "abc0123")) (DEdef "deKey" AN 0 Nothing 0 Nothing Nothing))
    , testCase "AN replacement escapes ?@':+ with ?" $
      assertEq (Right (DEStr "???@?'?:?+"))
      (testF (Just (DEStr "?@':+")) (DEdef "deKey" AN 0 Nothing 0 Nothing Nothing))
    , testCase "Bin replacement results in binary" $
      assertEq (Right (DEBinary "?@'"))
      (testF (Just (DEBinary "?@'")) (DEdef "deKey" Bin 0 Nothing 0 Nothing Nothing))
    , testCase "Code replacement results in string" $
      assertEq (Right (DEStr "123456"))
      (testF (Just (DEStr "123456")) (DEdef "deKey" Code 0 Nothing 0 Nothing Nothing))
    , testCase "Ctr replacement results in string" $
      assertEq (Right (DEStr "123456"))
      (testF (Just (DEStr "123456")) (DEdef "deKey" Ctr 0 Nothing 0 Nothing Nothing))
    , testCase "Cur replacement results in string" $
      assertEq (Right (DEStr "EUR"))
      (testF (Just (DEStr "EUR")) (DEdef "deKey" Cur 0 Nothing 0 Nothing Nothing))
    , testCase "DTAUS replacement results in binary" $
      assertEq (Right (DEBinary "SomethingSomething:?@'"))
      (testF (Just (DEBinary "SomethingSomething:?@'")) (DEdef "deKey" DTAUS 0 Nothing 0 Nothing Nothing))
    , testCase "Date replacement results in string" $
      assertEq (Right (DEStr "24.07.2014"))
      (testF (Just (DEStr "24.07.2014")) (DEdef "deKey" Date 0 Nothing 0 Nothing Nothing))
    , testCase "Dig replacement results in string" $
      assertEq (Right (DEStr "0123456789"))
      (testF (Just (DEStr "0123456789")) (DEdef "deKey" Dig 0 Nothing 0 Nothing Nothing))
    , testCase "ID replacement results in string" $
      assertEq (Right (DEStr "0123456789"))
      (testF (Just (DEStr "0123456789")) (DEdef "deKey" ID 0 Nothing 0 Nothing Nothing))
    , testCase "JN replacement results in string" $
      assertEq (Right (DEStr "J"))
      (testF (Just (DEStr "J")) (DEdef "deKey" JN 0 Nothing 0 Nothing Nothing))
    , testCase "Num replacement results in string" $
      assertEq (Right (DEStr "0123456789"))
      (testF (Just (DEStr "0123456789")) (DEdef "deKey" Num 0 Nothing 0 Nothing Nothing))
    , testCase "Time replacement results in string" $
      assertEq (Right (DEStr "12?:13?:14"))
      (testF (Just (DEStr "12:13:14")) (DEdef "deKey" Time 0 Nothing 0 Nothing Nothing))
    , testCase "Wrt replacement results in string" $
      assertEq (Right (DEStr "123,45"))
      (testF (Just (DEStr "123,45")) (DEdef "deKey" Time 0 Nothing 0 Nothing Nothing))
    , testCase "fillDe gives error if provided string too long" $
      assertEq (Left $ FillError ["deKey"] "Field has a maxsize of 5 but provided value '123456' has a length of 6")
      (testF (Just (DEStr "123456")) (DEdef "deKey" AN 0 (Just 5) 0 Nothing Nothing))
    , testCase "fillDe fills entry with 0s if provided Num too short" $
      assertEq (Right (DEStr "000123"))
      (testF (Just (DEStr "123")) (DEdef "deKey" Num 6 (Just 6) 0 Nothing Nothing))
    , testCase "fillDe gives error if values of other types are too short" $
      assertEq (Left $ FillError ["deKey"] "Field has a minsize of 6 but provided value '123' has a length of 3")
      (testF (Just (DEStr "123")) (DEdef "deKey" AN 6 (Just 6) 0 Nothing Nothing))
    , testCase "Check fillDe gives error if values of other types are too short" $
      assertEq (Left $ FillError ["deKey"] "Field has a minsize of 6 but provided value '123' has a length of 3")
      (testF (Just (DEStr "123")) (DEdef "deKey" AN 6 (Just 6) 0 Nothing Nothing))
    ]
  , testGroup "Test length calculations of DEs"
    [ testCase "Length of DEStr" $
      assertEq (Right (DEStr "abc", MkFillState 3 1))
      (testF2 Nothing (DEval (DEStr "abc")))
    , testCase "Length of DEBinary" $
      assertEq (Right (DEBinary "\0\0\0\0\0\0\0\0", MkFillState 11 1))
      (testF2 Nothing (DEval (DEBinary "\0\0\0\0\0\0\0\0")))
    , testCase "Length of 'seq'" $
      assertEq (Right (DEStr "123", MkFillState 3 123))
      (runStateT (fillDe Nothing (DEdef "seq" Num 0 (Just 3) 0 Nothing Nothing)) (MkFillState 0 123))
    ]
  ]
  where
    testF x y = evalStateT (fillDe x y) (MkFillState 0 1)

    testF2 x y = runStateT (fillDe x y) (MkFillState 0 1)


fillMsgTests :: [TF.Test]
fillMsgTests =
  [ testGroup "Simple message examples"
    [ testCase "One item message -- success" $
      assertEq (Right [[[DEStr "HNHBK"],[DEStr "000000000019"]]])
               (fmap fst $ finalizeMsg $ fillMsg
                (M.fromList [("MsgHead", M.fromList [("de1", DEentry $ DEStr "HNHBK")])])
                (MSG False False [SEG "MsgHead" False 0 Nothing [DEItem (DEdef "de1" AN 5 Nothing 1 (Just 1) Nothing)
                                                                 ,DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 (Just 1) Nothing)]]))
    , testCase "One item message -- No entries for equired SEG" $
      assertEq (Left "seg1.de1: Required DE missing in entries")
               (finalizeMsg $ fillMsg
                M.empty
                (MSG False False [SEG "seg1" False 1 Nothing [DEItem (DEdef "de1" AN 5 Nothing 1 (Just 1) Nothing)]]))
    -- , testCase "One item message -- missing msgsize field" $
    --   assertEq (Left ": Didn't find expected field message size")
    --            (fillMsg 1
    --             (M.fromList [("seg1", M.fromList [("de1", DEentry $ DEStr "abcxyz")])])
    --             (MSG False False [SEG "seg1" False 0 Nothing [DEItem (DEdef "de1" AN 5 Nothing 1 (Just 1) Nothing)]]))
    , testCase "One item message -- value already set 1" $
      assertEq (Right [[[DEStr "HNHBK"],[DEStr "000000000019"]]])
               (fmap fst $ finalizeMsg $ fillMsg
                M.empty
                (MSG False False [SEG "MsgHead" False 0 Nothing [DEItem (DEval (DEStr "HNHBK"))
                                                                 ,DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 (Just 1) Nothing)]]))
    , testCase "One item message -- value already set 2" $
      assertEq (Right [[[DEStr "HNHBK"],[DEStr "000000000019"]]])
               (fmap fst $ finalizeMsg $ fillMsg
                (M.fromList [("MsgHead", M.fromList [("de1", DEentry $ DEStr "SomethingOrOther")])])
                (MSG False False [SEG "MsgHead" False 0 Nothing [DEItem (DEval (DEStr "HNHBK"))
                                                                 ,DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 (Just 1) Nothing)]]))
    , testCase "One item message -- value outside of valids" $
      assertEq (Left "MsgHead.de1: Value '3' not in valid values '[\"1\",\"2\"]'")
               (finalizeMsg $ fillMsg
                (M.fromList [("MsgHead", M.fromList [("de1", DEentry $ DEStr "3")])])
                (MSG False False [SEG "MsgHead" False 0 Nothing [DEItem (DEdef "de1" AN 5 Nothing 1 (Just 1) (Just ["1","2"]))]]))
    , testCase "Message with one DEG with two DEs" $
      assertEq (Right [[[DEStr "99", DEStr "77"],[DEStr "000000000019"]]])
               (fmap fst $ finalizeMsg $ fillMsg
                (M.fromList [("MsgHead", M.fromList [("deg1", DEGentry $ M.fromList [("de1", DEStr "99")
                                                                                    ,("de2", DEStr "77")])])])
                (MSG False False
                 [SEG "MsgHead" False 0 Nothing
                   [DEGItem (DEG "deg1" 0 Nothing [DEdef "de1" AN 1 Nothing 1 (Just 1) Nothing
                                                  ,DEdef "de2" AN 2 Nothing 1 (Just 1) Nothing])
                   ,DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 (Just 1) Nothing)]]))
    , testCase "Empty segs are removed from message (no '')" $
      assertEq (Right [[[DEStr "1"]],[[DEStr "2"]]])
               (fmap fst $ finalizeMsg $ fillMsg
                (M.fromList [("MsgHead", M.fromList [("de1", DEentry $ DEStr "1")])
                            ,("MsgTail", M.fromList [("de2", DEentry $ DEStr "2")])])
                (MSG False False
                 [SEG "MsgHead"   False 0 Nothing [DEItem $ DEdef "de1" AN 0 Nothing 0 Nothing Nothing]
                 ,SEG "InBetween" False 0 Nothing []
                 ,SEG "MsgTail"   False 0 Nothing [DEItem $ DEdef "de2" AN 0 Nothing 0 Nothing Nothing]
                 ]))
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
      let vals =
            M.fromList [("Idn", M.fromList [("KIK", DEGentry $ M.fromList [("country", DEStr "280"), ("blz", DEStr "12030000")])])
                       ,("ProcPrep", M.fromList [("BPD", DEentry $ DEStr "3")
                                                ,("UPD", DEentry $ DEStr "2")
                                                ,("lang", DEentry $ DEStr "1")
                                                ,("prodName", DEentry $ DEStr "HsBCI")
                                                ,("prodVersion", DEentry $ DEStr "0.1.0")
                                                ])
                       ]
      in assertEq
         (Right "HNHBK:1:3+000000000109+220+0+1'HKIDN:2:2+280:12030000+9999999999+0+0'HKVVB:3:2+3+2+1+HsBCI+0.1.0'HNHBS:4:1+1'")
         (testF dialogInitAnon vals)
    , testCase "DialogInit" $
      let vals =
            M.fromList [("Idn", M.fromList [("KIK", DEGentry $ M.fromList [("country", DEStr "280"), ("blz", DEStr "12030000")])
                                           ,("customerid", DEentry $ DEStr "CUSTOMERID")
                                           ,("sysid", DEentry $ DEStr "0")
                                           ,("sysStatus", DEentry $ DEStr "1")])
                       ,("ProcPrep", M.fromList [("BPD", DEentry $ DEStr "3")
                                                ,("UPD", DEentry $ DEStr "2")
                                                ,("lang", DEentry $ DEStr "1")
                                                ,("prodName", DEentry $ DEStr "HsBCI")
                                                ,("prodVersion", DEentry $ DEStr "0.1.0")
                                                ])
                       ,("SigHead", M.fromList [("secfunc", DEentry $ DEStr "1")
                                               ,("seccheckref", DEentry $ DEStr "1234567890")
                                               ,("range", DEentry $ DEStr "1")
                                               ,("role", DEentry $ DEStr "1")
                                               ,("SecIdnDetails", DEGentry $ M.fromList [("func", DEStr "1")
                                                                                        ,("sysid", DEStr "0")
                                                                                        ,("cid", DEBinary "\0\0\0\0\0\0\0\0\0\0")])
                                               ,("secref", DEentry $ DEStr "1")
                                               ,("SecTimestamp", DEGentry $ M.fromList [("date", DEStr "20141111")
                                                                                       ,("time", DEStr "070809")])
                                               ,("HashAlg", DEGentry $ M.fromList [("alg", DEStr "999")])
                                               ,("SigAlg", DEGentry $ M.fromList [("alg", DEStr "10")
                                                                                 ,("mode", DEStr "16")])
                                               ,("KeyName", DEGentry $ M.fromList [("userid", DEStr "USERID9999")
                                                                                  ,("country", DEStr "280")
                                                                                  ,("blz", DEStr "12030000")
                                                                                  ,("keynum", DEStr "0")
                                                                                  ,("keyversion", DEStr "0")])
                                               ])
                       ,("SigTail", M.fromList [("UserSig", DEGentry $ M.fromList [("pin", DEStr "PIN12" )])
                                               ,("seccheckref", DEentry $ DEStr "1234567890")])
                       ]
          msg = testF dialogInit vals
          expectedMsg = "HNHBK:1:3+000000000249+220+0+1'HNSHK:2:3+1+1234567890+1+1+1:@10@\0\0\0\0\0\0\0\0\0\0:0+1+1:20141111:070809+1:999:1+6:10:16+280:12030000:USERID9999:S:0:0'HKIDN:3:2+280:12030000+CUSTOMERID+0+1'HKVVB:4:2+3+2+1+HsBCI+0.1.0'HNSHA:5:1+1234567890++PIN12'HNHBS:6:1+1'"
      in do assertEq msg (Right expectedMsg)
            assertEq 249 (BS.length expectedMsg)
    ]
  ]
  where
    testF msg vals = finalizeMsg (fillMsg vals msg) >>= return . gen . fst

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

third :: (t, t1, t2) -> t2
third (_,_,z) = z

extractSegTests :: [TF.Test]
extractSegTests =
  [ testGroup "Test extractSeg function"
    [ testCase "extractSeg expects a MsgHead" $
      assertEq (Left "Required element MsgHead not found") $
      extractSeg msg0 []
    , testCase "extractSeg gives an error if it can't find the element" $
      assertEq (Left "No definition for seg head HNHBK-3") $
      extractSeg msg0 [[DEStr "HNHBK", DEStr "1", DEStr "3"]]
    , testCase "Extract some entries from a single entry" $
      assertEq (Right ("MsgHead", Nothing, [("SegHead.seq",DEStr "1"),("msgsize",DEStr "000000000123")])) $
      extractSeg msg1 [[DEStr "HNHBK", DEStr "1", DEStr "3"],[DEStr "000000000123"]]
    , testCase "Extract multiple DEGs for a DEG with maxnum > 1" $
      assertEq (Right ("RetSeg", Nothing, [("deg1",DEStr "1"),("deg1",DEStr "2"),("deg1",DEStr "3")])) $
      extractSeg msg2 [[DEStr "HIRMS", DEStr "1", DEStr "1"],[DEStr "1"],[DEStr "2"],[DEStr "3"]]
    ]
  ]
  where
    sf0 = [SEG {segName = "MsgHead",
                needsRequestTag = False,
                segMinNum = 0,
                segMaxNum = Just 1,
                segItems = [DEGItem (DEG "" 1 (Just 1) [(DEval (DEStr "HNHBK")), (DEval (DEStr "whatever")), (DEval (DEStr "1"))])]}]
    sf1 = head $ msgItems $ dialogInitAnon

    msg0 = findSegDefs (MSG False False sf0)

    msg1 = findSegDefs (MSG False False [sf1])

    msg2 = findSegDefs (MSG False False [SEG {segName="RetSeg", needsRequestTag = False, segMinNum = 0, segMaxNum = Just 1
                                             ,segItems = [DEGItem (DEG "" 1 (Just 1)  [(DEval (DEStr "HIRMS")), (DEval (DEStr "1")), (DEval (DEStr "1"))])
                                                         ,DEGItem (DEG "" 1 (Just 10) [DEdef "deg1" AN 0 Nothing 0 Nothing Nothing])
                                                         ]}])

extractMsgTests :: [TF.Test]
extractMsgTests =
  [ testGroup "Test extractMsg function"
    [ testCase "Extract from generated 'DialogInitAnon'" $
      let -- keys    = ["Idn.KIK.country", "Idn.KIK.blz", "ProcPrep.BPD", "ProcPrep.UPD", "ProcPrep.lang", "ProcPrep.prodName", "ProcPrep.prodVersion"]
          -- vals    = map DEStr ["280",  "12030000", "3", "2", "1", "HsBCI", "0.1.0"]
          inputs  = M.fromList [("Idn", M.fromList [("KIK", DEGentry $ M.fromList [("country", DEStr "280"), ("blz", DEStr "12030000")])])
                               ,("ProcPrep", M.fromList [("BPD", DEentry $ DEStr "3")
                                                        ,("UPD", DEentry $ DEStr "2")
                                                        ,("lang", DEentry $ DEStr "1")
                                                        ,("prodName", DEentry $ DEStr "HsBCI")
                                                        ,("prodVersion", DEentry $ DEStr "0.1.0")])]
          retVals = do msg <- finalizeMsg $ fillMsg inputs dialogInitAnon
                       return $ extractMsg dialogInitAnon $ fst msg
                       -- return $ fmap snd $ concat $ concat $ catMaybes [M.lookup k (msgDataBySegName matched) | k <- keys]
      in assertEq
         (Right ([],MsgData {msgDataBySegName = M.fromList [("Idn",[[("SegHead.seq",DEStr "2")
                                                                    ,("KIK.country",DEStr "280")
                                                                    ,("KIK.blz",DEStr "12030000")]])
                                                           ,("MsgHead",[[("SegHead.seq",DEStr "1")
                                                                        ,("msgsize",DEStr "000000000109")]])
                                                           ,("MsgTail",[[("SegHead.seq",DEStr "4")]])
                                                           ,("ProcPrep",[[("SegHead.seq",DEStr "3")
                                                                         ,("BPD",DEStr "3")
                                                                         ,("UPD",DEStr "2")
                                                                         ,("lang",DEStr "1")
                                                                         ,("prodName",DEStr "HsBCI")
                                                                         ,("prodVersion",DEStr "0.1.0")]])]
                            ,msgDataBySegRef = IM.fromList []}))
         retVals
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
                         , extractSegTests
                         , extractMsgTests
                         ]

xmlTests :: [[Content] -> TF.Test]
xmlTests = getMSGfromXMLTest

main :: IO ()
main = do
  hbciPlus <- getXml "resources/hbci-plus.xml"
  defaultMain (standaloneTests ++ map ($ hbciPlus) xmlTests)

dialogInitAnon :: MSG
dialogInitAnon =
  (MSG False False [(SEG "MsgHead" False 1 (Just 1) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNHBK")),
                                                                                            (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                            (DEval (DEStr "3")),
                                                                                            (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                        (DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 Nothing Nothing)),
                                                        (DEItem (DEval (DEStr "220"))),
                                                        (DEItem (DEval (DEStr "0"))),
                                                        (DEItem (DEval (DEStr "1"))),
                                                        (DEGItem (DEG "MsgRef" 0 (Just 1) [(DEdef "dialogid" ID 0 Nothing 1 Nothing Nothing),
                                                                                           (DEdef "msgnum" Num 0 (Just 4) 1 Nothing Nothing)]))]),
                  (SEG "Idn" False 1 (Just 1) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HKIDN")),
                                                                                        (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                        (DEval (DEStr "2")),
                                                                                        (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                    (DEGItem (DEG "KIK" 1 (Just 1) [(DEdef "country" Ctr 0 Nothing 1 Nothing Nothing),
                                                                                    (DEdef "blz" AN 0 (Just 30) 0 Nothing Nothing)])),
                                                    (DEItem (DEval (DEStr "9999999999"))),
                                                    (DEItem (DEval (DEStr "0"))),
                                                    (DEItem (DEval (DEStr "0")))]),
                  (SEG "ProcPrep" False 1 (Just 1) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HKVVB")),
                                                                                             (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                             (DEval (DEStr "2")),
                                                                                             (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                         (DEItem (DEdef "BPD" Num 0 (Just 3) 1 Nothing Nothing)),
                                                         (DEItem (DEdef "UPD" Num 0 (Just 3) 1 Nothing Nothing)),
                                                         (DEItem (DEdef "lang" Num 0 (Just 3) 1 Nothing (Just ["0",
                                                                                                               "1",
                                                                                                               "2",
                                                                                                               "3"]))),
                                                         (DEItem (DEdef "prodName" AN 0 (Just 25) 1 Nothing Nothing)),
                                                         (DEItem (DEdef "prodVersion" AN 0 (Just 5) 1 Nothing Nothing))]),
                  (SEG "MsgTail" False 1 (Just 1) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNHBS")),
                                                                                            (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                            (DEval (DEStr "1")),
                                                                                            (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                        (DEItem (DEval (DEStr "1")))])])


dialogInit :: MSG
dialogInit =
  (MSG True True [(SEG "MsgHead" False 1 (Just 1) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNHBK")),
                                                                                       (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                       (DEval (DEStr "3")),
                                                                                       (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                   (DEItem (DEdef "msgsize" Dig 12 (Just 12) 1 Nothing Nothing)),
                                                   (DEItem (DEval (DEStr "220"))),
                                                   (DEItem (DEval (DEStr "0"))),
                                                   (DEItem (DEval (DEStr "1"))),
                                                   (DEGItem (DEG "MsgRef" 0 (Just 1) [(DEdef "dialogid" ID 0 Nothing 1 Nothing Nothing),
                                                                                      (DEdef "msgnum" Num 0 (Just 4) 1 Nothing Nothing)]))]),
                  (SEG "SigHead" False 0 (Just 3) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNSHK")),
                                                                                       (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                       (DEval (DEStr "3")),
                                                                                       (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                   (DEItem (DEdef "secfunc" AN 0 (Just 3) 1 Nothing Nothing)),
                                                   (DEItem (DEdef "seccheckref" AN 0 (Just 14) 1 Nothing Nothing)),
                                                   (DEItem (DEval (DEStr "1"))),
                                                   (DEItem (DEdef "role" AN 0 (Just 3) 1 Nothing (Just ["1",
                                                                                                        "3",
                                                                                                        "4"]))),
                                                   (DEGItem (DEG "SecIdnDetails" 1 (Just 1) [(DEdef "func" AN 0 (Just 3) 1 Nothing (Just ["1",
                                                                                                                                          "2"])),
                                                                                             (DEdef "cid" Bin 0 (Just 256) 0 Nothing Nothing),
                                                                                             (DEdef "sysid" ID 0 Nothing 0 Nothing Nothing)])),
                                                   (DEItem (DEdef "secref" Num 0 (Just 16) 1 Nothing Nothing)),
                                                   (DEGItem (DEG "SecTimestamp" 1 (Just 1) [(DEval (DEStr "1")),
                                                                                            (DEdef "date" Date 0 Nothing 0 Nothing Nothing),
                                                                                            (DEdef "time" Time 0 Nothing 0 Nothing Nothing)])),
                                                   (DEGItem (DEG "HashAlg" 1 (Just 1) [(DEval (DEStr "1")),
                                                                                       (DEdef "alg" AN 0 (Just 3) 1 Nothing (Just ["999"])),
                                                                                       (DEval (DEStr "1")),
                                                                                       (DEdef "parameter" Bin 0 (Just 512) 0 Nothing Nothing)])),
                                                   (DEGItem (DEG "SigAlg" 1 (Just 1) [(DEval (DEStr "6")),
                                                                                      (DEdef "alg" AN 0 (Just 3) 1 Nothing (Just ["1",
                                                                                                                                  "10"])),
                                                                                      (DEdef "mode" AN 0 (Just 999) 1 Nothing (Just ["999",
                                                                                                                                     "16"]))])),
                                                   (DEGItem (DEG "KeyName" 1 (Just 1) [(DEdef "country" Ctr 0 Nothing 1 Nothing Nothing),
                                                                                       (DEdef "blz" AN 0 (Just 30) 0 Nothing Nothing),
                                                                                       (DEdef "userid" ID 0 Nothing 1 Nothing Nothing),
                                                                                       (DEval (DEStr "S")),
                                                                                       (DEdef "keynum" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                       (DEdef "keyversion" Num 0 (Just 3) 1 Nothing Nothing)])),
                                                   (DEGItem (DEG "Cert" 0 (Just 1) [(DEdef "type" Num 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                  "2",
                                                                                                                                  "3"])),
                                                                                    (DEdef "cert" Bin 0 (Just 2048) 1 Nothing Nothing)]))]),
                  (SEG "Idn" False 1 (Just 1) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HKIDN")),
                                                                                   (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                   (DEval (DEStr "2")),
                                                                                   (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                               (DEGItem (DEG "KIK" 1 (Just 1) [(DEdef "country" Ctr 0 Nothing 1 Nothing Nothing),
                                                                               (DEdef "blz" AN 0 (Just 30) 0 Nothing Nothing)])),
                                               (DEItem (DEdef "customerid" ID 0 Nothing 1 Nothing Nothing)),
                                               (DEItem (DEdef "sysid" ID 0 Nothing 1 Nothing Nothing)),
                                               (DEItem (DEdef "sysStatus" Num 0 (Just 1) 1 Nothing (Just ["0",
                                                                                                          "1"])))]),
                  (SEG "ProcPrep" False 1 (Just 1) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HKVVB")),
                                                                                        (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                        (DEval (DEStr "2")),
                                                                                        (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                    (DEItem (DEdef "BPD" Num 0 (Just 3) 1 Nothing Nothing)),
                                                    (DEItem (DEdef "UPD" Num 0 (Just 3) 1 Nothing Nothing)),
                                                    (DEItem (DEdef "lang" Num 0 (Just 3) 1 Nothing (Just ["0",
                                                                                                          "1",
                                                                                                          "2",
                                                                                                          "3"]))),
                                                    (DEItem (DEdef "prodName" AN 0 (Just 25) 1 Nothing Nothing)),
                                                    (DEItem (DEdef "prodVersion" AN 0 (Just 5) 1 Nothing Nothing))]),
                  (SEG "KeyReq" False 0 (Just 2) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HKISA")),
                                                                                      (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                      (DEval (DEStr "2")),
                                                                                      (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                  (DEItem (DEval (DEStr "2"))),
                                                  (DEItem (DEval (DEStr "124"))),
                                                  (DEGItem (DEG "KeyName" 1 (Just 1) [(DEdef "country" Ctr 0 Nothing 1 Nothing Nothing),
                                                                                      (DEdef "blz" AN 0 (Just 30) 0 Nothing Nothing),
                                                                                      (DEdef "userid" ID 0 Nothing 1 Nothing Nothing),
                                                                                      (DEdef "keytype" AN 0 (Just 1) 1 Nothing (Just ["S",
                                                                                                                                      "V"])),
                                                                                      (DEdef "keynum" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                      (DEdef "keyversion" Num 0 (Just 3) 1 Nothing Nothing)])),
                                                  (DEGItem (DEG "Cert" 0 (Just 1) [(DEdef "type" Num 0 (Just 1) 1 Nothing (Just ["1",
                                                                                                                                 "2",
                                                                                                                                 "3"])),
                                                                                   (DEdef "cert" Bin 0 (Just 2048) 1 Nothing Nothing)]))]),
                  (SEG "SigTail" False 0 (Just 3) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNSHA")),
                                                                                       (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                       (DEval (DEStr "1")),
                                                                                       (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                   (DEItem (DEdef "seccheckref" AN 0 (Just 14) 1 Nothing Nothing)),
                                                   (DEItem (DEdef "sig" Bin 0 (Just 512) 0 Nothing Nothing)),
                                                   (DEGItem (DEG "UserSig" 0 (Just 1) [(DEdef "pin" AN 0 (Just 99) 1 Nothing Nothing),
                                                                                       (DEdef "tan" AN 0 (Just 35) 0 Nothing Nothing)]))]),
                  (SEG "MsgTail" False 1 (Just 1) [(DEGItem (DEG "SegHead" 1 (Just 1) [(DEval (DEStr "HNHBS")),
                                                                                       (DEdef "seq" Num 0 (Just 3) 1 Nothing Nothing),
                                                                                       (DEval (DEStr "1")),
                                                                                       (DEdef "ref" Num 0 (Just 3) 0 Nothing Nothing)])),
                                                   (DEItem (DEval (DEStr "1")))])])
