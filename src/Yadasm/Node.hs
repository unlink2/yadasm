module Yadasm.Node where

import qualified Data.ByteString as B
import qualified Yadasm.Line as L
import qualified Yadasm.Symbol as S
import qualified Yadasm.Binary as Bi
import           Yadasm.Context
import           Data.Maybe (isNothing, isJust)

data Node =
  Node { children :: [Node]
       , reader :: B.ByteString -> Integer
       , size :: Int
       , converter :: Integer -> Int -> Maybe ([L.CodeWord], [S.Symbol])
       , comparator :: Integer -> Bool
       }

instance Eq Node where
  (==) n1 n2 = size n1 == size n2 && children n1 == children n2

instance Show Node where
  show node = "{Node}" ++ show (children node)

appendParsed :: Maybe ([a1], [a2]) -> Maybe ([a1], [a2]) -> Maybe ([a1], [a2])
appendParsed (Just (pw, ps)) (Just (ow, os)) = Just (ow ++ pw, os ++ ps)
appendParsed _ _ = Nothing

parseChildren :: Context
              -> B.ByteString
              -> [Node]
              -> Maybe ([L.CodeWord], [S.Symbol])
              -> Maybe ([L.CodeWord], [S.Symbol])
parseChildren ctx bin [] prev = prev
parseChildren ctx bin (node:nodes) prev
  | isNothing parsed = Nothing
  | otherwise =
    parseChildren ctx (B.drop siz bin) nodes (appendParsed parsed prev)
  where
    parsed = parse ctx bin node

    siz = size node

parse :: Context -> B.ByteString -> Node -> Maybe ([L.CodeWord], [S.Symbol])
parse ctx bin node =
  if comp dat
  then parseChildren ctx (B.drop siz bin) nodes (convert dat siz)
  else Nothing
  where
    nodes = children node

    comp = comparator node

    read = reader node

    convert = converter node

    dat = read bin

    siz = size node
