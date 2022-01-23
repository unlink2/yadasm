module Yadasm.TestParser where

import           Test.HUnit
import qualified Yadasm.Parser as P
import           Test.HUnit.Lang (assertEqual)
import           Yadasm.TestNode (testNode1, testNode2, testNode3)
import qualified Yadasm.Parser as N
import           Data.HashMap.Lazy as HashMap

tests = [ TestCase
            (assertEqual
               "Should build lookup"
               (HashMap.fromList
                  [(1 :: Integer, testNode1), (2, testNode2), (4, testNode3)])
               (N.buildLookup [testNode1, testNode2, testNode3] 6))
        , TestCase
            (assertEqual
               "Should build lookup and ignore unused cases"
               (HashMap.fromList [(1 :: Integer, testNode1), (2, testNode2)])
               (N.buildLookup [testNode1, testNode2, testNode3] 2))]

