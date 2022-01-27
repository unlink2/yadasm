module Yadasm.Plugins.InsertWord where

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

insertWord
  :: (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString)
  -> HashMap Integer ([L.CodeWord], [L.CodeWord])
  -> (C.Context -> Maybe ([L.CodeWord], [L.CodeWord]))
  -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString)
insertWord (Just (words, symbols), ctx, bin) wordsMap defaultComment =
  ( Just (addWords words (HashMap.lookup (C.address ctx) wordsMap), symbols)
  , ctx
  , bin)
  where
    addWords words (Just (before, after)) = before ++ words ++ after
    addWords words Nothing
      | isJust (defaultComment ctx) = addWords words (defaultComment ctx)
      | otherwise = words
insertWord result wordsMap defaultComment = result

parseInsertWord'
  :: (C.Context -> Maybe ([L.CodeWord], [L.CodeWord]))
  -> (C.Context
      -> ByteString
      -> HashMap Integer N.Node
      -> Maybe N.Node
      -> (ByteString -> Integer)
      -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString))
  -> HashMap Integer ([L.CodeWord], [L.CodeWord])
  -> C.Context
  -> ByteString
  -> HashMap Integer N.Node
  -> Maybe N.Node
  -> (ByteString -> Integer)
  -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString)
parseInsertWord' defaultComment parse words ctx bin nodes defaultNode readOp =
  insertWord (parse ctx bin nodes defaultNode readOp) words defaultComment

noDefaultComment :: p -> Maybe a
noDefaultComment ctx = Nothing

addressComment :: String -> C.Context -> Maybe ([L.CodeWord], [L.CodeWord])
addressComment commentPrefix ctx = Just
  ( []
  , [ L.defaultCodeWord { L.text = commentPrefix ++ showHex (C.address ctx) ""
                        }])

parseInsertWord
  :: (C.Context
      -> ByteString
      -> HashMap Integer N.Node
      -> Maybe N.Node
      -> (ByteString -> Integer)
      -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString))
  -> HashMap Integer ([L.CodeWord], [L.CodeWord])
  -> C.Context
  -> ByteString
  -> HashMap Integer N.Node
  -> Maybe N.Node
  -> (ByteString -> Integer)
  -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString)
parseInsertWord = parseInsertWord' noDefaultComment
