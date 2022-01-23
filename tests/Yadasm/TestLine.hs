module Yadasm.TestLine where

import           Test.HUnit
import           Yadasm.Line

tests :: [Test]
tests =
  [ TestCase
      (assertEqual
         "it should calculate total size of codewords"
         4
         (totalSize
            [defaultCodeWord { size = 1 }, defaultCodeWord { size = 3 }]))]
