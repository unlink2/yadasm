module Yadasm.Plugins.AlAs65C816 where

-- This plugin swaps between 
-- !as and !al when it sees sep #$20 or rep #$20 
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
import qualified Data.HashSet as HashSet

asNodes :: HashMap Integer N.Node
asNodes = P.buildLookup A65C816.nodesEmulated 0xFF

insertAsIf :: Bool -> P.ParseRes -> P.ParseRes
insertAsIf True (Just (words, symbols), ctx, bin) =
  ( Just (words ++ [L.defaultCodeWord { L.text = "\n!as" }], symbols)
  , C.setFlag "as" "true" ctx
  , bin)
insertAsIf cond result = result

insertAlIf :: Bool -> P.ParseRes -> P.ParseRes
insertAlIf True (Just (words, symbols), ctx, bin) =
  ( Just (words ++ [L.defaultCodeWord { L.text = "\n!al" }], symbols)
  , C.unsetFlag "as" ctx
  , bin)
insertAlIf cond result = result

-- not pretty, but it covers the only automatic cases 
-- we care about
modifyResult :: P.ParseRes -> P.ParseRes
modifyResult (Just (words, symbols), ctx, bin) = insertAlIf
  (words
   == [ L.defaultCodeWord { L.text = "rep ", L.raw = 0xC2, L.size = 1 }
      , L.defaultCodeWord { L.text = "#$20", L.raw = 0x20, L.size = 1 }])
  (insertAsIf
     (words
      == [ L.defaultCodeWord { L.text = "sep ", L.raw = 0xE2, L.size = 1 }
         , L.defaultCodeWord { L.text = "#$20", L.raw = 0x20, L.size = 1 }])
     (Just (words, symbols), ctx, bin))
modifyResult result = result

force :: (Bool -> P.ParseRes -> P.ParseRes)
      -> HashSet.HashSet Integer
      -> P.ParseRes
      -> P.ParseRes
force modifyResult forceAt (response, ctx, bin) =
  modifyResult (HashSet.member (C.address ctx) forceAt) (response, ctx, bin)

forceAl :: HashSet.HashSet Integer -> P.ParseRes -> P.ParseRes
forceAl = force insertAlIf

forceAs :: HashSet.HashSet Integer -> P.ParseRes -> P.ParseRes
forceAs = force insertAsIf

parseAlAs' :: (P.ParseRes -> P.ParseRes)
           -> P.ParseFn
           -> C.Context
           -> ByteString
           -> HashMap Integer N.Node
           -> Maybe N.Node
           -> B.ReadOp
           -> P.ParseRes
parseAlAs' modifyResult parse ctx bin nodes defaultNode readOp
  | isJust (C.lookupFlag "as" ctx) =
    modifyResult $ parse ctx bin asNodes defaultNode readOp
  | otherwise = modifyResult $ parse ctx bin nodes defaultNode readOp

parseAlAs :: P.ParseFn
          -> C.Context
          -> ByteString
          -> HashMap Integer N.Node
          -> Maybe N.Node
          -> B.ReadOp
          -> P.ParseRes
parseAlAs = parseAlAs' modifyResult

parseForceAs :: HashSet.HashSet Integer
             -> P.ParseFn
             -> C.Context
             -> ByteString
             -> HashMap Integer N.Node
             -> Maybe N.Node
             -> B.ReadOp
             -> P.ParseRes
parseForceAs forceAt = parseAlAs' (forceAs forceAt)

parseForceAl :: HashSet.HashSet Integer
             -> P.ParseFn
             -> C.Context
             -> ByteString
             -> HashMap Integer N.Node
             -> Maybe N.Node
             -> B.ReadOp
             -> P.ParseRes
parseForceAl forceAt = parseAlAs' (forceAl forceAt)
