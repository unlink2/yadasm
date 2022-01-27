module Yadasm.Archs.ArchRaw where

-- raw byte collecting architecture 
import           Data.Bits
import qualified Yadasm.Node as N
import qualified Yadasm.Binary as B
import qualified Data.ByteString as ByteString
import qualified Yadasm.Symbol as S
import qualified Yadasm.Line as L
import qualified Yadasm.Context as C
import qualified Yadasm.Comparator as Cmp
import qualified Data.Char as Char
import           Text.Printf
import           Data.Maybe (isNothing)

rawConverter :: C.Context -> Integer -> Int -> L.NodeResult
rawConverter ctx dat size = Just
  ( [ L.defaultCodeWord { L.raw = dat
                        , L.size = 1
                        , L.text = [Char.chr (fromIntegral dat)]
                        }]
  , [])

readByteNode :: N.Node
readByteNode = N.defaultNode { N.reader = B.read1le
                             , N.converter = rawConverter
                             , N.size = 1
                             }

nodes :: [N.Node]
nodes = [readByteNode]
