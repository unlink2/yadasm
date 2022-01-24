module Yadasm.TestBinary where

import qualified Yadasm.Binary as Binary
import qualified Data.ByteString as B
import           Test.HUnit
import           Test.HUnit.Lang (assertEqual)

tests =
  [ TestCase
      (assertEqual
         "It should slice from start to end"
         (B.pack [2, 0, 3])
         (Binary.slice 2 3 (B.pack [1, 0, 2, 0, 3, 4, 5])))
  , TestCase
      (assertEqual
         "It should slice end"
         (B.pack [1, 0, 2])
         (Binary.sliceEnd 3 (B.pack [1, 0, 2, 0, 3, 4, 5])))
  , TestCase
      (assertEqual
         "It should allow slicing past the end"
         (B.pack [1, 0, 1])
         (Binary.sliceEnd 5 (B.pack [1, 0, 1])))
  , TestCase
      (assertEqual
         "It should read singe byte"
         (Prelude.toInteger 1)
         (Binary.read1le (B.pack [1, 2, 3])))
  , TestCase
      (assertEqual
         "It should read 2 bytes"
         (Prelude.toInteger 513)
         (Binary.read2le (B.pack [1, 2, 3])))
  , TestCase
      (assertEqual
         "It should read 3 bytes"
         (Prelude.toInteger 197121)
         (Binary.read3le (B.pack [1, 2, 3])))]
