module Yadasm.TestParser where

import           Test.HUnit
import qualified Yadasm.Parser as P
import           Test.HUnit.Lang (assertEqual)
import qualified Yadasm.TestNode as TN
import           Data.HashMap.Lazy as HashMap
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Binary as Bin
import qualified Yadasm.Line as L
import qualified Yadasm.Symbol as S
import qualified Yadasm.Node as N
import qualified Yadasm.Error as E

testNode1 = TN.testNode1 { N.size = 1 }

testNode2 = TN.testNode2 { N.size = 1 }

testNode3 = TN.testNode3 { N.size = 1 }

testLookup = P.buildLookup [testNode1, testNode2, testNode3] 6

testContext = C.defaultContext { C.address = 0x100, C.endAddress = 0x110 }

testData = ByteString.pack [2, 3, 1, 1, 2, 3, 1]

tests =
  [ TestCase
      (assertEqual
         "Should build lookup"
         (HashMap.fromList
            [(1 :: Integer, testNode1), (2, testNode2), (4, testNode3)])
         (P.buildLookup [testNode1, testNode2, testNode3] 6))
  , TestCase
      (assertEqual
         "Should build lookup and ignore unused cases"
         (HashMap.fromList [(1 :: Integer, testNode1), (2, testNode2)])
         (P.buildLookup [testNode1, testNode2, testNode3] 2))
  , TestCase
      (assertEqual
         "Should find correct node for opcode with no default"
         (Just testNode1)
         (P.lookupNodeOr
            1
            Nothing
            (P.buildLookup [testNode1, testNode2, testNode3] 2)))
  , TestCase
      (assertEqual
         "Should find not correct node for opcode with no default"
         Nothing
         (P.lookupNodeOr
            5
            Nothing
            (P.buildLookup [testNode1, testNode2, testNode3] 5)))
  , TestCase
      (assertEqual
         "Should find default node for opcode"
         (Just testNode3)
         (P.lookupNodeOr
            5
            (Just testNode3)
            (P.buildLookup [testNode1, testNode2] 5)))
  , TestCase
      (assertEqual
         "Should find default node for opcode with default"
         (Just testNode1)
         (P.lookupNodeOr
            1
            (Just testNode3)
            (P.buildLookup [testNode1, testNode2] 2)))
  , TestCase
      (assertEqual
         "It should parse input node"
         ( Just
             ( [ L.CodeWord { L.text = "test2"
                            , L.size = 1
                            , L.raw = 0
                            , L.attr = L.Std
                            }
               , L.CodeWord { L.text = "test3"
                            , L.size = 2
                            , L.raw = 0
                            , L.attr = L.Std
                            }]
             , [S.defaultSymbol { S.address = 0x100, S.name = "Test3:" }])
         , testContext
         , testData)
         (P.parse testContext testData testLookup Nothing Bin.read1le))
  , TestCase
      (assertEqual
         "It should fail with error"
         ( Nothing
         , testContext { C.cerror = Just E.ParserError }
         , ByteString.pack [4, 2])
         (P.parse
            testContext
            (ByteString.pack [4, 2])
            testLookup
            Nothing
            Bin.read1le))
  , TestCase
      (assertEqual
         "It should build a symbol table"
         testContext { C.address = 0x100
                     , C.symbols = HashMap.fromList
                         [ ( 256
                           , [ S.defaultSymbol { S.address = 256
                                               , S.name = "Test3:"
                                               }])
                         , ( 260
                           , [ S.defaultSymbol { S.address = 260
                                               , S.name = "Test3:"
                                               }])]
                     }
         (P.buildSymbolTable
            testContext
            testData
            testLookup
            Nothing
            Bin.read1le
            P.parse))]

