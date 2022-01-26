module Yadasm.Archs.TestArch65C02 where

import           Test.HUnit
import qualified Yadasm.Archs.Arch65C02 as A65C02
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin
import qualified Yadasm.Archs.TestArch6502

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup A65C02.nodes 0xFF

-- the 65C02 tests should pass all 6502 tests too!
tests = tests' testMap

tests' testMap =
  [ TestCase
      (assertEqual
         "It should parse zp indirect"
         (Just ["ora ($AA)"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x12, 0xAA])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse jump abs, x"
         (Just ["jmp (label_BBAA, x)"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x7C, 0xAA, 0xBB])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse bit ext"
         (Just ["bit #$AA", "bit $BB, x", "bit $CCDD, x"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x89, 0xAA, 0x34, 0xBB, 0x3C, 0xDD, 0xCC])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse tsb"
         (Just ["tsb $AA", "tsb $BBAA"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x04, 0xAA, 0x0C, 0xAA, 0xBB])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse tsb"
         (Just ["trb $AA", "trb $BBAA"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x14, 0xAA, 0x1C, 0xAA, 0xBB])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse stz"
         (Just ["stz $AA, x", "stz $BBAA, x", "stz $11", "stz $2211"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [0x74, 0xAA, 0x9E, 0xAA, 0xBB, 0x64, 0x11, 0x9C, 0x11, 0x22])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse bra"
         (Just ["bra label_5AC"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x80, 0xAA])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse default node when incomplete opdoce is input"
         (Just ["nop", "!byte $80"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xEA, 0x80])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse Nothing when incomplete code is input and no default node is supplied"
         Nothing
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xEA, 0x80])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse single byte"
         (Just ["inc A", "dec A", "phy", "ply", "phx", "plx"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xFA])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            P.parse))]
  ++ Yadasm.Archs.TestArch6502.tests' testMap
