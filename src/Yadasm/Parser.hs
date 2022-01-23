module Yadasm.Parser where

import           Data.HashMap.Lazy as HashMap
import qualified Yadasm.Node as N
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Yadasm.Context as C
import qualified Yadasm.Symbol as S
import qualified Yadasm.Line as L
import           Data.Maybe (isNothing, isJust)
import qualified Yadasm.Binary

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

lookupNodeOr
  :: Integer -> Maybe N.Node -> HashMap Integer N.Node -> Maybe N.Node
lookupNodeOr opcode (Just defaultNode) nodes = Just
  (HashMap.lookupDefault defaultNode opcode nodes)
lookupNodeOr opcode Nothing nodes = HashMap.lookup opcode nodes

-- calculates the size in bytes of a parser result
totalParsedSize :: Maybe ([L.CodeWord], [S.Symbol]) -> Int
totalParsedSize Nothing = 0
totalParsedSize (Just (words, symbols)) = L.totalSize words

advanceBin :: ByteString -> Maybe ([L.CodeWord], [S.Symbol]) -> ByteString
advanceBin bin Nothing = bin
advanceBin bin (Just next) = ByteString.drop (totalParsedSize (Just next)) bin

advanceCtx :: C.Context -> Maybe ([L.CodeWord], [S.Symbol]) -> C.Context
advanceCtx ctx Nothing = ctx
advanceCtx ctx (Just next) =
  C.advance (toInteger (totalParsedSize (Just next))) ctx

-- parses the entire input and builds a symbol table
-- does not keep actual parsed lines 
buildSymbols
  :: C.Context
  -> ByteString
  -> HashMap Integer N.Node
  -> Maybe N.Node
  -> (ByteString -> Integer)
  -> (C.Context
      -> ByteString
      -> HashMap Integer N.Node
      -> Maybe N.Node
      -> (ByteString -> Integer)
      -> Maybe ([L.CodeWord], [S.Symbol]))
  -> C.Context
buildSymbols ctx bin nodes defaultNode readOp parseFn
  | ByteString.null bin = ctx
  | isJust next = buildSymbols
    (advanceCtx (addSymbolsFromParsed ctx next) next)
    (advanceBin bin next)
    nodes
    defaultNode
    readOp
    parseFn
  | otherwise = ctx
  where
    next = parseFn ctx bin nodes defaultNode readOp

    addSymbolsFromParsed
      :: C.Context -> Maybe ([L.CodeWord], [S.Symbol]) -> C.Context
    addSymbolsFromParsed ctx Nothing = ctx
    addSymbolsFromParsed ctx (Just (words, symbol:symbols)) =
      addSymbolsFromParsed (C.addSymbol ctx symbol) (Just (words, symbols))
    addSymbolsFromParsed ctx _ = ctx

-- parses the next valid node  
-- based on the provided lookup and symbol table
parse :: C.Context
      -> ByteString
      -> HashMap Integer N.Node
      -> Maybe N.Node
      -> (ByteString -> Integer)
      -> Maybe ([L.CodeWord], [S.Symbol])
parse ctx bin nodes defaultNode readOp = parseNode node
  where
    node = lookupNodeOr (readOp bin) defaultNode nodes

    parseNode Nothing = Nothing
    parseNode (Just node) = N.parse ctx bin node
