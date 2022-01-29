module Yadasm.Plugins.TestChainParser where

import           Test.HUnit
import qualified Yadasm.Archs.Arch65C816 as A65C816
import qualified Yadasm.Archs.Arch6502 as A6502
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
import qualified Yadasm.Plugins.ChainParser as PP

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup A65C816.nodes 0xFF

testMap6502 = P.buildLookup A6502.nodes 0xFF

tests =
  [ TestCase
      (assertEqual
         "It should chain-parse and fall back to regular parser's default node"
         (Just ["!byte $5B"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x5B])
            testMap6502
            (Just A6502H.defaultNode)
            Bin.read1le
            (PP.chainParse testMap6502 P.parse)))
  , TestCase
      (assertEqual
         "It should chain-parse and not fall back to regular parser"
         (Just ["tcd"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0x5B])
            testMap6502
            (Just A6502H.defaultNode)
            Bin.read1le
            (PP.chainParse testMap P.parse)))
  , TestCase
      (assertEqual
         "Chain-parser should have presedence over regular parser (1)"
         (Just ["lda #$5BAA"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xA9, 0xAA, 0x5B])
            testMap6502
            (Just A6502H.defaultNode)
            Bin.read1le
            (PP.chainParse testMap P.parse)))
  , TestCase
      (assertEqual
         "Chain-parser should have presedence over regular parser (2)"
         (Just ["lda #$AA", "tcd"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xA9, 0xAA, 0x5B])
            testMap
            (Just A6502H.defaultNode)
            Bin.read1le
            (PP.chainParse testMap6502 P.parse)))]
