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
pad resultToString padding padWith (Just (words, symbols), ctx, bin) =
  ( Just
      ( makePadding (resultToString ctx (Just (words, symbols))) padding
      , symbols)
  , ctx
  , bin)
  where
    makePadding (Just str) padding
      | padding > 0 = words
        ++ [ L.defaultCodeWord { L.text = concat
                                   $ replicate
                                     (max 0 (padding - length str))
                                     [padWith]
                               }]
      | padding < 0 =
        L.defaultCodeWord { L.text = concat
                              $ replicate
                                (max 0 (abs (padding + length str)))
                                [padWith]
                          }
        :words
      | padding == 0 = words
    makePadding Nothing padding = []
pad resultToString padding padWith (Nothing, ctx, bin) = (Nothing, ctx, bin)

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
parsePadLineTo'
  resultToString
  padding
  padWith
  parse
  ctx
  bin
  nodes
  defaultNode
  readOp =
  pad resultToString padding padWith (parse ctx bin nodes defaultNode readOp)

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
