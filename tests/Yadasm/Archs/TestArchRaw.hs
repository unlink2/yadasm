module Yadasm.Archs.TestArchRaw where

import           Test.HUnit
import qualified Yadasm.Archs.ArchRaw as Araw
import qualified Yadasm.Context as C
import qualified Data.ByteString as ByteString
import qualified Yadasm.Node as N
import qualified Yadasm.Parser as P
import qualified Yadasm.Binary as Bin
import qualified Yadasm.Definition as D
import qualified Data.HashMap.Lazy as HashMap

testContext = C.defaultContext { C.address = 0x600 }

testMap = P.buildLookup Araw.nodes 0xFF

tests =
  [ TestCase
      (assertEqual
         "It should look up definitions"
         (Just ["\169", "D", "\173", "\NUL", "\255", "U", "D", "\169", "E"])
         (P.parseAllToStringSymbolTable
            testContext
            (ByteString.pack
               [0xA9, 0x44, 0xAD, 0x00, 0xFF, 0x55, 0x44, 0xA9, 0x45])
            testMap
            Nothing
            Bin.read1le
            P.parse))]

