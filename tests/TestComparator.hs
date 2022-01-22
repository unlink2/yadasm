module TestComparator where

import qualified Comparator
import           Test.HUnit

tests =
  [ TestCase (assertBool "Test should always be true" Comparator.alwaysTrue)
  , TestCase
      (assertBool "It should always be false" (not Comparator.alwaysFalse))]
