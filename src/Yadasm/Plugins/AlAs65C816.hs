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
  :: (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString)
  -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString)
modifyResult (Just (words, symbols), ctx, bin)
  | words
    == [ L.defaultCodeWord { L.text = "sep ", L.raw = 0xE2, L.size = 1 }
       , L.defaultCodeWord { L.text = "#$20", L.raw = 0x20, L.size = 1 }] =
    ( Just (words ++ [L.defaultCodeWord { L.text = "\n!as" }], symbols)
    , ctx
    , bin)
  | words
    == [ L.defaultCodeWord { L.text = "rep ", L.raw = 0xC2, L.size = 1 }
       , L.defaultCodeWord { L.text = "#$20", L.raw = 0x20, L.size = 1 }] =
    ( Just (words ++ [L.defaultCodeWord { L.text = "\n!al" }], symbols)
    , ctx
    , bin)
modifyResult result = result

parseAlAs
  :: (C.Context
      -> ByteString
      -> HashMap Integer N.Node
      -> Maybe N.Node
      -> (ByteString -> Integer)
      -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString))
  -> C.Context
  -> ByteString
  -> HashMap Integer N.Node
  -> Maybe N.Node
  -> (ByteString -> Integer)
  -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString)
parseAlAs parse ctx bin nodes defaultNode readOp =
  modifyResult $ parse ctx bin nodes defaultNode readOp
