module Yadasm.TestLine where

import           Test.HUnit
import           Yadasm.Line
import           Yadasm.TestContext
import qualified Yadasm.Context as C

tests :: [Test]
tests =
  [ TestCase
      (assertEqual
         "it should calculate total size of codewords"
         4
         (totalSize
            [defaultCodeWord { size = 1 }, defaultCodeWord { size = 3 }]))
  , TestCase
      (assertEqual
         "It should output formatted result for lines"
         (Just "3:\nlda #$10")
         (resultToString
            testContext { C.address = 0x101 }
            (Just
               ( [ defaultCodeWord { text = "lda " }
                 , defaultCodeWord { text = "#$10" }]
               , []))))
  , TestCase
      (assertEqual
         "It should return an empty string on empty input"
         Nothing
         (resultToString testContext { C.address = 0x101 } Nothing))]
