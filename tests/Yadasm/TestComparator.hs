module Yadasm.TestComparator where

import qualified Yadasm.Comparator as Comparator
import           Test.HUnit

tests :: [Test]
tests =
  [ TestCase
      (assertBool "Test should always be true" $ Comparator.alwaysTrue 1)
  , TestCase
      (assertBool "It should always be false" (not $ Comparator.alwaysFalse 1))]
