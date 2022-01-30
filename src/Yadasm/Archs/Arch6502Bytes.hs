module Yadasm.Archs.Arch6502Bytes where

-- custom byte based parser nodes 
-- can be assembeled to custom parser 
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Node as N
import qualified Yadasm.Line as L

readByteNode :: L.Attrs -> [N.Node] -> N.Node
readByteNode = A6502H.readByteNode' "!byte " ""

readWordNode :: L.Attrs -> [N.Node] -> N.Node
readWordNode = A6502H.readWordNode' "!word " ""

readLWordNode :: L.Attrs -> [N.Node] -> N.Node
readLWordNode = A6502H.readLWordNode' "!le24 " ""

readStringNode :: Int -> L.Attrs -> N.Node
readStringNode len attr = A6502H.readCharNode
  "!text \""
  ""
  attr
  (replicate (len - 2) (A6502H.readCharNode "" "" L.Std [])
   ++ [A6502H.readCharNode "" "\"" L.Std []])

-- this is meant to be a builder 
-- for a chain of opcodes that translate to a 
-- macro call
-- currently macro definitions need to be 
-- inserted by hand 
readMacroNode' :: String -> String -> Integer -> [N.Node] -> N.Node
readMacroNode' prefix name = A6502H.opcodeNode (prefix ++ name)

-- default macro call 
readMacroNode :: String -> Integer -> [N.Node] -> N.Node
readMacroNode = readMacroNode' "+"
