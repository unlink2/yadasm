module Yadasm.Parser where

import           Data.HashMap.Lazy as HashMap
import qualified Yadasm.Node as N

addNode map opcode (Just node) = HashMap.insert (toInteger opcode) node map
addNode map opcode Nothing = map

checkOpcode
  :: HashMap Integer N.Node -> N.Node -> Int -> HashMap Integer N.Node
checkOpcode map node (-1) = map
checkOpcode map node opcode
  | N.comparator node (toInteger opcode) = addNode map opcode (Just node)
  | otherwise = checkOpcode map node (opcode - 1)

checkNode
  :: HashMap Integer N.Node -> [N.Node] -> Int -> Int -> HashMap Integer N.Node
checkNode map [] opcode maxOp = map
checkNode map (node:nodes) (-1) maxOp = checkNode map nodes maxOp maxOp
checkNode map (node:nodes) opcode maxOp =
  checkNode (checkOpcode map node opcode) (node:nodes) (opcode - 1) maxOp

-- iterate until we are out of opcodes or nodes
buildLookup'
  :: HashMap Integer N.Node -> [N.Node] -> Int -> HashMap Integer N.Node
buildLookup' map nodes opcode = checkNode map nodes opcode opcode

-- builds a lookup HashMap for a node set 
-- by matching the first element (the opcode) with a comparator
buildLookup :: [N.Node] -> Int -> HashMap Integer N.Node
buildLookup = buildLookup' (HashMap.fromList [])

parse = Nothing
