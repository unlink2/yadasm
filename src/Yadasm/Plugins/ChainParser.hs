module Yadasm.Plugins.ChainParser where

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
import qualified Yadasm.Binary as B

chainParse :: HashMap Integer N.Node
           -> P.ParseFn
           -> C.Context
           -> ByteString
           -> HashMap Integer N.Node
           -> Maybe N.Node
           -> B.ReadOp
           -> P.ParseRes
chainParse altNodes parse ctx bin nodes defaultNode readOp =
  parsePreOr parsePre
  where
    parsePre = parse ctx bin altNodes Nothing readOp

    parseReg = parse ctx bin nodes defaultNode readOp

    parsePreOr (Just result, ctx, bin) = (Just result, ctx, bin)
    parsePreOr (Nothing, ctx, bin) = parseReg
