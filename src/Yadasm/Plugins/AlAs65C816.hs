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

-- not pretty, but it covers the only automatic cases 
-- we care about
modifyResult
  :: Maybe ([L.CodeWord], [S.Symbol]) -> Maybe ([L.CodeWord], [S.Symbol])
modifyResult (Just (words, symbols))
  | length words == 2
    && head words
    == L.defaultCodeWord { L.text = "sep", L.raw = 0xE2, L.size = 1 }
    && head (tail words)
    == L.defaultCodeWord { L.text = "#$20", L.raw = 0x20, L.size = 1 } =
    Just (L.defaultCodeWord { L.text = "!as\n" }:words, symbols)
  | length words == 2
    && head words
    == L.defaultCodeWord { L.text = "rep", L.raw = 0xC2, L.size = 1 }
    && head (tail words)
    == L.defaultCodeWord { L.text = "#$20", L.raw = 0x20, L.size = 1 } =
    Just (L.defaultCodeWord { L.text = "!al\n" }:words, symbols)
modifyResult result = result

parseAlAs :: (C.Context
              -> ByteString
              -> HashMap Integer N.Node
              -> Maybe N.Node
              -> (ByteString -> Integer)
              -> Maybe ([L.CodeWord], [S.Symbol]))
          -> C.Context
          -> ByteString
          -> HashMap Integer N.Node
          -> Maybe N.Node
          -> (ByteString -> Integer)
          -> Maybe ([L.CodeWord], [S.Symbol])
parseAlAs parse ctx bin nodes defaultNode readOp =
  modifyResult $ parse ctx bin nodes defaultNode readOp
