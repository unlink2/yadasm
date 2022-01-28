module Yadasm.Plugins.TestPadLineTo where

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
import qualified Yadasm.Plugins.PadLineTo as PT

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup A65C816.nodes 0xFF

tests =
  [ TestCase
      (assertEqual
         "It should pad to length"
         (Just
            [ "nop#########"
            , "nop#########"
            , "lda #$EAEA##"
            , "lda #$EAEA##"
            , "nop#########"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA])
            testMap
            Nothing
            Bin.read1le
            (PT.parsePadLineTo 12 '#' P.parse)))
  , TestCase
      (assertEqual
         "It should pad to length, but not more"
         (Just
            [ "nop#######"
            , "nop#######"
            , "lda #$EAEA"
            , "lda #$EAEA"
            , "nop#######"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA])
            testMap
            Nothing
            Bin.read1le
            (PT.parsePadLineTo 10 '#' P.parse)))
  , TestCase
      (assertEqual
         "It should not pad at all when 0"
         (Just ["nop", "nop", "lda #$EAEA", "lda #$EAEA", "nop"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA])
            testMap
            Nothing
            Bin.read1le
            (PT.parsePadLineTo 0 '#' P.parse)))
  , TestCase
      (assertEqual
         "It should not pad at all when invalid code is provided"
         Nothing
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA])
            testMap
            Nothing
            Bin.read1le
            (PT.parsePadLineTo 0 '#' P.parse)))
  , TestCase
      (assertEqual
         "It should pad to length (less than 0)"
         (Just
            [ "#########nop"
            , "#########nop"
            , "##lda #$EAEA"
            , "##lda #$EAEA"
            , "#########nop"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA])
            testMap
            Nothing
            Bin.read1le
            (PT.parsePadLineTo (-12) '#' P.parse)))
  , TestCase
      (assertEqual
         "It should pad to length, but not more (less than 0)"
         (Just
            [ "#######nop"
            , "#######nop"
            , "lda #$EAEA"
            , "lda #$EAEA"
            , "#######nop"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xA9, 0xEA, 0xEA, 0xEA])
            testMap
            Nothing
            Bin.read1le
            (PT.parsePadLineTo (-10) '#' P.parse)))]

