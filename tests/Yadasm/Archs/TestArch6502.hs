module Yadasm.Archs.TestArch6502 where

import           Test.HUnit
import qualified Yadasm.Archs.Arch6502 as A6502
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin

testContext = C.defaultContext { C.address = 0x100 }

testMap = P.buildLookup A6502.nodes 0xFF

tests = [ TestCase
            (assertEqual
               "It should parse immediate"
               (Just ["lda #$AB ", "lda $AB "])
               (P.parseAllToString
                  testContext
                  (ByteString.pack [0xA9, 0xAB, 0xA5, 0xAB])
                  testMap
                  Nothing
                  Bin.read1le))]

