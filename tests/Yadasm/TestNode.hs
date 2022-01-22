module Yadasm.TestNode where

import           Test.HUnit (assertEqual, Test(TestCase))
import qualified Yadasm.Binary as Bi
import qualified Yadasm.Comparator as Comp
import qualified Yadasm.Context as Ctx
import qualified Data.ByteString as B
import qualified Yadasm.Line as L
import qualified Yadasm.Symbol as S
import qualified Yadasm.Node as N

-- simple test code converter, reader and comparator 
testComparator :: (Ord a, Num a) => p -> a -> Bool
testComparator i size = size == 1

testConverter :: Integer -> Int -> Maybe ([L.CodeWord], [S.Symbol])
testConverter 1 size =
  Just ([L.CodeWord { L.text = "test1", L.size = size }], [])
testConverter 2 size = Just
  ( [L.CodeWord { L.text = "test2", L.size = size }]
  , [ S.Symbol { S.address = 1
               , S.name = "Test2:"
               , S.shadow = False
               , S.order = 0
               }])
testConverter _ size = Nothing

testNode1 = N.Node { N.children = []
                   , N.reader = Bi.read1le
                   , N.converter = testConverter
                   , N.comparator = testComparator
                   , N.size = 1
                   }

testNode2 = testNode1 { N.children = [ N.Node { N.children = []
                                              , N.reader = Bi.read1le
                                              , N.converter = testConverter
                                              , N.comparator = testComparator
                                              , N.size = 1
                                              }]
                      }

tests :: [Test]
tests =
  [ TestCase
      (assertEqual
         "It should parse valid input"
         (Just ([L.CodeWord { L.text = "test1", L.size = 1 }], []))
         (N.parse Ctx.defaultContext (B.pack [1]) testNode1))
  , TestCase
      (assertEqual
         "It should parse valid input with children"
         (Just
            ( [ L.CodeWord { L.text = "test1", L.size = 1 }
              , L.CodeWord { L.text = "test2", L.size = 1 }]
            , [ S.Symbol { S.address = 1
                         , S.name = "Test2:"
                         , S.shadow = False
                         , S.order = 0
                         }]))
         (N.parse Ctx.defaultContext (B.pack [1, 2]) testNode2))
  , TestCase
      (assertEqual
         "It should fail when not enough data is available"
         Nothing
         (N.parse Ctx.defaultContext (B.pack []) testNode1))
  , TestCase
      (assertEqual
         "It should fail when not enough data is available with children"
         Nothing
         (N.parse Ctx.defaultContext (B.pack [1]) testNode2))]
