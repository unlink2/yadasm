module Yadasm.Plugins.TestAlAs65C816 where

import           Test.HUnit
import qualified Yadasm.Archs.Arch65C816 as A65C816
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin
import qualified Yadasm.Archs.TestArch6502
import qualified Yadasm.Plugins.AlAs65C816 as AlAs

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup A65C816.nodes 0xFF

tests =
  [ TestCase
      (assertEqual
         "It should use al as plugin"
         (Just
            [ "!al"
            , "rep #$20"
            , "lda #$EAEA"
            , "!as"
            , "sep #$20"
            , "lda #$EA"
            , "!al"
            , "lda #$EAEA"
            , "!as"
            , "lda #$EA"
            , "rep #$20"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [ 0xC2
               , 0x20
               , 0xA9
               , 0xEA
               , 0xEA
               , 0xE2
               , 0x20
               , 0xA9
               , 0xEA
               , 0xA9
               , 0xEA
               , 0xEA
               , 0xA9
               , 0xEA
               , 0xC2
               , 0x20])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            (AlAs.parseAlAs P.parse)))]
