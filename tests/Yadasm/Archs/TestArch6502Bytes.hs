module Yadasm.Archs.TestArch6502Bytes where

import           Test.HUnit
import qualified Yadasm.Archs.Arch6502Bytes as A6502B
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin
import qualified Yadasm.Definition as D
import qualified Data.HashMap.Lazy as HashMap
import qualified Yadasm.Line as L

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup
  [ A6502B.readLWordNode
      L.Std
      [A6502B.readWordNode L.NewLine [A6502B.readByteNode L.NewLine []]]]
  0xFF

testStringMap = P.buildLookup [A6502B.readStringNode 6 L.Std] 0xFF

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
            P.parse))]
