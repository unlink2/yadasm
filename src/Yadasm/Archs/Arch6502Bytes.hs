module Yadasm.Archs.Arch6502Bytes where

-- custom byte based parser nodes 
-- can be assembeled to custom parser 
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Node as N

readByteNode :: Bool -> [N.Node] -> N.Node
readByteNode = A6502H.readByteNode' "!byte " ""

readWordNode :: Bool -> [N.Node] -> N.Node
readWordNode = A6502H.readWordNode' "!word " ""

readLWordNode :: Bool -> [N.Node] -> N.Node
readLWordNode = A6502H.readLWordNode' "!le24 " ""
