module Yadasm.Archs.TestArch6502 where

import           Test.HUnit
import qualified Yadasm.Archs.Arch6502 as A6502
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup A6502.nodes 0xFF

tests = tests' testMap

tests' testMap =
  [ TestCase
      (assertEqual
         "It should parse immediate"
         (Just
            [ "lda #$AB"
            , "lda $AB"
            , "lda $AB, x"
            , "lda $0200"
            , "lda $0200, x"
            , "lda $0200, y"
            , "lda ($AB, x)"
            , "lda ($AB), y"])
         (P.parseAllToString
            testContext
            (ByteString.pack
               [ 0xA9
               , 0xAB
               , 0xA5
               , 0xAB
               , 0xB5
               , 0xAB
               , 0xAD
               , 0x00
               , 0x02
               , 0xBD
               , 0x00
               , 0x02
               , 0xB9
               , 0x00
               , 0x02
               , 0xA1
               , 0xAb
               , 0xB1
               , 0xAB])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse logic"
         (Just ["asl A", "asl $44", "asl $44, x", "asl $4400", "asl $4400, x"])
         (P.parseAllToString
            testContext
            (ByteString.pack
               [ 0x0A
               , 0x06
               , 0x44
               , 0x16
               , 0x44
               , 0x0E
               , 0x00
               , 0x44
               , 0x1E
               , 0x00
               , 0x44])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse bit"
         (Just ["bit $44", "bit $4400"])
         (P.parseAllToString
            testContext
            (ByteString.pack [0x24, 0x44, 0x2C, 0x00, 0x44])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse branch"
         (Just
            [ "label_600:\nbrk"
            , "label_601:\nbrk"
            , "brk"
            , "bpl label_600"
            , "bpl label_601"
            , "bpl label_60C"
            , "brk"
            , "brk"
            , "brk"
            , "label_60C:\nbpl label_5EE"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [ 0x00
               , 0x00
               , 0x00
               , 0x10
               , 0xFB
               , 0x10
               , 0xFA
               , 0x10
               , 0x03
               , 0x00
               , 0x00
               , 0x00
               , 0x10
               , 0xE0])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse compare index"
         (Just ["cpx #$44", "cpx $44", "cpx $4400"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xE0, 0x44, 0xE4, 0x44, 0xEC, 0x00, 0x44])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse dec"
         (Just ["dec $44", "dec $44, x", "dec $4400", "dec $4400, x"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [0xC6, 0x44, 0xD6, 0x44, 0xCE, 0x00, 0x44, 0xDE, 0x00, 0x44])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse compare jmp"
         (Just ["jmp label_603", "label_603:\njmp (label_5420)"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x4C, 0x03, 0x06, 0x6C, 0x20, 0x54])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse jsr"
         (Just ["jsr label_5420"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x20, 0x20, 0x54])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse ldx"
         (Just
            ["ldx #$44", "ldx $44", "ldx $44, y", "ldx $4455", "ldx $4455, y"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [ 0xA2
               , 0x44
               , 0xA6
               , 0x44
               , 0xB6
               , 0x44
               , 0xAE
               , 0x55
               , 0x44
               , 0xBE
               , 0x55
               , 0x44])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse ldy"
         (Just
            ["ldy #$44", "ldy $44", "ldy $44, x", "ldy $4455", "ldy $4455, x"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [ 0xA0
               , 0x44
               , 0xA4
               , 0x44
               , 0xB4
               , 0x44
               , 0xAC
               , 0x55
               , 0x44
               , 0xBC
               , 0x55
               , 0x44])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse stx"
         (Just ["stx $44", "stx $44, y", "stx $4455"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x86, 0x44, 0x96, 0x44, 0x8E, 0x55, 0x44])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse sty"
         (Just ["sty $44", "sty $44, x", "sty $4455"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x84, 0x44, 0x94, 0x44, 0x8C, 0x55, 0x44])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse default"
         (Just ["!byte $FF"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xFF])
            Yadasm.Archs.TestArch6502.testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should fail parse without default"
         Nothing
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xFF])
            Yadasm.Archs.TestArch6502.testMap
            Nothing
            Bin.read1le
            P.parse))]

