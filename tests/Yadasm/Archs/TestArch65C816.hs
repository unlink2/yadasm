module Yadasm.Archs.TestArch65C816 where

import           Test.HUnit
import qualified Yadasm.Archs.Arch65C02 as A65C02
import qualified Yadasm.Archs.Arch65C816 as A65C816
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin
import qualified Yadasm.Archs.TestArch65C02

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup A65C816.nodes 0xFF

testMapEmu = P.buildLookup A65C816.nodesEmulated 0xFF

tests =
  [ TestCase
      (assertEqual
         "It should parse long branch and immediate"
         (Just ["lda #$3412", "brl label_56F"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xA9, 0x12, 0x34, 0x82, 0x6A, 0xFF])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse extended instructions"
         (Just
            [ "ora $11, s"
            , "ora [$22]"
            , "ora $554433"
            , "ora ($66, s), y"
            , "ora [$77], y"
            , "ora $CCBBAA, x"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [ 0x03
               , 0x11
               , 0x07
               , 0x22
               , 0x0F
               , 0x33
               , 0x44
               , 0x55
               , 0x13
               , 0x66
               , 0x17
               , 0x77
               , 0x1F
               , 0xAA
               , 0xBB
               , 0xCC])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse xce"
         (Just ["xce"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xFB])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse cop"
         (Just ["cop #$12"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x02, 0x12])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse long jump"
         (Just ["jsl label_452312", "jsl label_452312"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x22, 0x12, 0x23, 0x45, 0x22, 0x12, 0x23, 0x45])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse per"
         (Just ["per label_2312"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x62, 0x12, 0x23])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse rep and sep"
         (Just ["rep #$12", "sep #$13"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xC2, 0x12, 0xE2, 0x13])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse move"
         (Just ["mvp #$12, #$E2"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x44, 0x12, 0xE2])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse pei"
         (Just ["pei $12"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xD4, 0x12])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse pea"
         (Just ["pea #$1112"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xF4, 0x12, 0x11])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse jml"
         (Just ["jmp [label_1112]"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xDC, 0x12, 0x11])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse jsrx"
         (Just ["jsr (label_1112, x)"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xFC, 0x12, 0x11])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))]
  ++ Yadasm.Archs.TestArch65C02.tests' testMapEmu
