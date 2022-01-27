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

asNodes :: HashMap Integer N.Node
asNodes = P.buildLookup A65C816.nodesEmulated 0xFF

-- not pretty, but it covers the only automatic cases 
-- we care about
modifyResult :: P.ParseRes -> P.ParseRes
modifyResult (Just (words, symbols), ctx, bin)
  | words
    == [ L.defaultCodeWord { L.text = "sep ", L.raw = 0xE2, L.size = 1 }
       , L.defaultCodeWord { L.text = "#$20", L.raw = 0x20, L.size = 1 }] =
    ( Just (words ++ [L.defaultCodeWord { L.text = "\n!as" }], symbols)
    , C.setFlag "as" "true" ctx
    , bin)
  | words
    == [ L.defaultCodeWord { L.text = "rep ", L.raw = 0xC2, L.size = 1 }
       , L.defaultCodeWord { L.text = "#$20", L.raw = 0x20, L.size = 1 }] =
    ( Just (words ++ [L.defaultCodeWord { L.text = "\n!al" }], symbols)
    , C.unsetFlag "as" ctx
    , bin)
modifyResult result = result

parseAlAs :: P.ParseFn
          -> C.Context
          -> ByteString
          -> HashMap Integer N.Node
          -> Maybe N.Node
          -> B.ReadOp
          -> P.ParseRes
parseAlAs parse ctx bin nodes defaultNode readOp
  | isJust (C.lookupFlag "as" ctx) =
    modifyResult $ parse ctx bin asNodes defaultNode readOp
  | otherwise = modifyResult $ parse ctx bin nodes defaultNode readOp
