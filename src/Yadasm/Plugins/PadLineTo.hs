module Yadasm.Plugins.PadLineTo where

import           Data.HashMap.Lazy as HashMap
import qualified Yadasm.Node as N
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Yadasm.Context as C
import qualified Yadasm.Symbol as S
import qualified Yadasm.Line as L
import           Data.Maybe (isNothing, isJust)
import qualified Yadasm.Binary
import qualified Yadasm.Archs.Arch65C816 as A65C816
import qualified Yadasm.Parser as P
import           Numeric (showHex, showIntAtBase)
import qualified Yadasm.Binary as B

pad :: (C.Context -> L.NodeResult -> Maybe String)
    -> Int
    -> Char
    -> P.ParseRes
    -> P.ParseRes
pad resultToString padding padWith (result, ctx, bin) = (result, ctx, bin)

parsePadLineTo'
  :: (C.Context -> L.NodeResult -> Maybe String)
  -> Int
  -> Char
  -> P.ParseFn
  -> C.Context
  -> ByteString
  -> HashMap Integer N.Node
  -> Maybe N.Node
  -> B.ReadOp
  -> P.ParseRes
parsePadLineTo' resultToString padding padWith parse = parse

parsePadLineTo
  :: Int
  -> Char
  -> P.ParseFn
  -> C.Context
  -> ByteString
  -> HashMap Integer N.Node
  -> Maybe N.Node
  -> B.ReadOp
  -> P.ParseRes
parsePadLineTo = parsePadLineTo' L.resultToString
