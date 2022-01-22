module Yadasm.Parser where

import           Data.HashMap.Lazy as HashMap
import qualified Yadasm.Node as N

-- iterate until we are out of opcodes or nodes
buildLookup'
  :: HashMap Integer N.Node -> [N.Node] -> Int -> HashMap Integer N.Node
buildLookup' map [] opcode = map
buildLookup' map nodes (-1) = map
buildLookup' map (node:nodes) opcode
  | comparator (toInteger opcode) = buildLookup'
    (HashMap.insert (toInteger opcode) node map)
    nodes
    (opcode - 1)
  | otherwise = buildLookup' map nodes opcode
  where
    comparator = N.comparator node

-- builds a lookup HashMap for a node set 
-- by matching the first element (the opcode) with a comparator
buildLookup :: [N.Node] -> Int -> HashMap Integer N.Node
buildLookup = buildLookup' (HashMap.fromList [])

parse = Nothing
