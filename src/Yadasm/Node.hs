module Yadasm.Node where

import qualified Data.ByteString as B
import qualified Yadasm.Line as L
import qualified Yadasm.Symbol as S
import qualified Yadasm.Binary as Bi
import qualified Yadasm.Context as C
import qualified Yadasm.Comparator as Co
import           Yadasm.Context
import           Data.Maybe (isNothing, isJust)

type NodeComparator = Integer -> Bool

type NodeConverter = C.Context -> Integer -> Int -> L.NodeResult

data Node = Node { children :: [Node]
                 , reader :: Bi.ReadOp
                 , size :: Int
                 , converter :: NodeConverter
                 , comparator :: NodeComparator
                 }

instance Eq Node where
  (==) n1 n2 = size n1 == size n2 && children n1 == children n2

instance Show Node where
  show node = "{Node:" ++ show (size node) ++ show (children node) ++ "}"

convertNothing ctx input size = Nothing

defaultNode :: Node
defaultNode = Node { children = []
                   , reader = Bi.read1le
                   , size = 0
                   , converter = convertNothing
                   , comparator = Co.alwaysTrue
                   }

appendParsed :: Maybe ([a1], [a2]) -> Maybe ([a1], [a2]) -> Maybe ([a1], [a2])
appendParsed (Just (pw, ps)) (Just (ow, os)) = Just (ow ++ pw, os ++ ps)
appendParsed _ _ = Nothing

parseChildren
  :: Context -> B.ByteString -> [Node] -> L.NodeResult -> L.NodeResult
parseChildren ctx bin [] prev = prev
parseChildren ctx bin (node:nodes) prev
  | isNothing parsed = Nothing
  | otherwise =
    parseChildren ctx (B.drop siz bin) nodes (appendParsed parsed prev)
  where
    parsed = parse ctx bin node

    siz = size node

parse :: Context -> B.ByteString -> Node -> L.NodeResult
parse ctx bin node =
  if comp dat && B.length bin >= siz
  then parseChildren ctx (B.drop siz bin) nodes (convert ctx dat siz)
  else Nothing
  where
    nodes = children node

    comp = comparator node

    read = reader node

    convert = converter node

    dat = read bin

    siz = size node
