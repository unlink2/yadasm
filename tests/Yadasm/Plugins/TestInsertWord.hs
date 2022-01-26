module Yadasm.Plugins.TestInsertWord where

import           Test.HUnit
import qualified Yadasm.Archs.Arch65C816 as A65C816
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Lazy as HashMap
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin
import qualified Yadasm.Line as L
import qualified Yadasm.Symbol as S
import qualified Yadasm.Archs.TestArch6502
import qualified Yadasm.Plugins.InsertWord as IW

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup A65C816.nodes 0xFF

tests =
  [ TestCase
      (assertEqual
         "It should use insert line plugin"
         (Just ["nop", "nop", "lda #$EAEA ; insertAfter", "lda #$EAEA"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            (IW.parseInsertWord
               P.parse
               (HashMap.fromList
                  [(0x602, [L.defaultCodeWord { L.text = " ; insertAfter" }])]))))]
