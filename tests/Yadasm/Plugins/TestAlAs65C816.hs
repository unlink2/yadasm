module Yadasm.Plugins.TestAlAs65C816 where

import           Test.HUnit
import qualified Yadasm.Archs.Arch65C816 as A65C816
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin
import qualified Yadasm.Line as L
import qualified Yadasm.Symbol as S
import qualified Yadasm.Archs.TestArch6502
import qualified Yadasm.Plugins.AlAs65C816 as AlAs
import qualified Data.HashSet as HashSet

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup A65C816.nodes 0xFF

tests =
  [ TestCase
      (assertEqual
         "It should use al as plugin"
         (Just
            [ "rep #$20\n!al"
            , "lda #$EAEA"
            , "sep #$20\n!as"
            , "lda #$EA"
            , "rep #$20\n!al"
            , "lda #$EAEA"
            , "sep #$20\n!as"
            , "lda #$EA"
            , "rep #$20\n!al"])
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
               , 0xC2
               , 0x20
               , 0xA9
               , 0xEA
               , 0xEA
               , 0xE2
               , 0x20
               , 0xA9
               , 0xEA
               , 0xC2
               , 0x20])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            (AlAs.parseAlAs P.parse)))
  , TestCase
      (assertEqual
         "It should force al and as"
         (Just
            [ "rep #$20\n!al"
            , "lda #$EAEA"
            , "sep #$20\n!as"
            , "lda #$EA"
            , "rep #$20\n!al"
            , "lda #$EAEA"
            , "sep #$20\n!as"
            , "lda #$EA"
            , "rep #$20\n!al"])
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
               , 0xC2
               , 0x20
               , 0xA9
               , 0xEA
               , 0xEA
               , 0xE2
               , 0x20
               , 0xA9
               , 0xEA
               , 0xC2
               , 0x20])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            (AlAs.parseForceAs
               (HashSet.fromList [0x605, 0x60E])
               (AlAs.parseForceAl
                  (HashSet.fromList [0x600, 0x609, 0x612])
                  P.parse))))]
