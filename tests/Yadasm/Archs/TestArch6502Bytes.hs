module Yadasm.Archs.TestArch6502Bytes where

import           Test.HUnit
import qualified Yadasm.Archs.Arch6502Bytes as A6502B
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin
import qualified Yadasm.Definition as D
import qualified Data.HashMap.Lazy as HashMap
import qualified Yadasm.Line as L
import qualified Yadasm.Archs.Arch6502Bytes as A602H

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup
  [ A6502B.readLWordNode
      L.Std
      [A6502B.readWordNode L.NewLine [A6502B.readByteNode L.NewLine []]]]
  0xFF

testStringMap = P.buildLookup [A6502B.readStringNode 6 L.Std] 0xFF

-- macro lda #$x lda #$y
testMacroMap = P.buildLookup
  [ A6502B.readMacroNode
      "testMacro"
      0xA9
      [ A6502H.readByteNode
      , A6502H.consumeByteNode 0xA9 []
      , A6502H.appendStringNode ", "
      , A6502H.readByteNode]]
  0xFF

tests =
  [ TestCase
      (assertEqual
         "It read custom structure"
         (Just
            [ "!le24 $111111\n!word $2222\n!byte $33"
            , "!le24 $111111\n!word $2222\n!byte $33"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [ 0x11
               , 0x11
               , 0x11
               , 0x22
               , 0x22
               , 0x33
               , 0x11
               , 0x11
               , 0x11
               , 0x22
               , 0x22
               , 0x33])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should fail if out of data"
         Nothing
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [ 0x11
               , 0x11
               , 0x11
               , 0x22
               , 0x22
               , 0x33
               , 0x11
               , 0x11
               , 0x11
               , 0x22
               , 0x22])
            testMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should read string"
         (Just ["!text \"Hello\0\""])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [72, 101, 108, 108, 111, 00])
            testStringMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should parse macros"
         (Just ["+testMacro $11, $22"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xA9, 0x11, 0xA9, 0x22])
            testMacroMap
            Nothing
            Bin.read1le
            P.parse))
  , TestCase
      (assertEqual
         "It should not parse invalid macros"
         Nothing
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack [0xA9, 0x11, 0xA8, 0x22])
            testMacroMap
            Nothing
            Bin.read1le
            P.parse))]
