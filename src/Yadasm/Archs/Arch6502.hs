module Yadasm.Archs.Arch6502 where

import           Data.Bits
import qualified Yadasm.Node as N
import qualified Yadasm.Binary as B
import qualified Data.ByteString as ByteString
import qualified Yadasm.Symbol as S
import qualified Yadasm.Line as L
import qualified Yadasm.Context as C
import qualified Yadasm.Comparator as Cmp
import           Text.Printf

data InstructionMode =
    Immediate
  | ZeroPage
  | ZeroPageX
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | IndirectX
  | IndirectY
  | Accumulator
  | Implied
  | Relative
  | AbsoluteJump
  | ZeropageY
  deriving (Eq, Show)

-- simply returns the node's name as a codeword
textConverter
  :: String -> C.Context -> Integer -> Int -> Maybe ([L.CodeWord], [S.Symbol])
textConverter text ctx dat size =
  Just ([L.defaultCodeWord { L.text = text, L.size = size }], [])

numConverter
  :: String -> C.Context -> Integer -> Int -> Maybe ([L.CodeWord], [S.Symbol])
numConverter fmt ctx dat size =
  Just ([L.defaultCodeWord { L.text = printf fmt dat, L.size = size }], [])

opcodeComparator :: Eq a => a -> a -> Bool
opcodeComparator expected opcode = expected == opcode

opcodeNode :: String -> Integer -> [N.Node] -> N.Node
opcodeNode name opcode children =
  N.defaultNode { N.reader = B.read1le
                , N.converter = textConverter name
                , N.comparator = opcodeComparator opcode
                , N.children = children
                , N.size = 1
                }

readByteNode = N.defaultNode { N.reader = B.read1le
                             , N.converter = numConverter "$%02X"
                             , N.size = 1
                             }

readImmediateNode =
  readByteNode { N.converter = numConverter "#$%02X", N.size = 1 }

readWordNode = N.defaultNode { N.reader = B.read2le
                             , N.converter = numConverter "$%04X"
                             , N.size = 2
                             }

readRelLabelNode = N.defaultNode

readAbsLabelNode = N.defaultNode

-- take an opcode and extract the CC bits 
extractCCBits opcode = opcode .&. 0x03 -- 0b00000011

-- mask out unwanted parts of an opcode to apply an addressing mode
applyMask opcode = opcode .&. 0x1C -- 0b00011100

-- extract remainder of opcode without addressing mode 
-- information 
extractOpcode opcode = opcode .&. 0xE3 -- 0b11100011 

-- turns an opcode into the desired addressing mode 
-- depending on the cc bits
mask :: InstructionMode -> Integer -> Integer
mask Immediate opcode
  | extractCCBits opcode == 0x01 = applyMask 0x69 .|. extractOpcode opcode
mask ZeroPage opcode
  | extractCCBits opcode == 0x01 = applyMask 0x65 .|. extractOpcode opcode
mask _ opcode = 0xFF -- TODO remove 

-- create an instruction from the input functions 
-- and the supplied data 
makeInstruction
  :: (String -> Integer -> [N.Node] -> N.Node)
  -> N.Node
  -> N.Node
  -> N.Node
  -> N.Node
  -> N.Node
  -> [N.Node]
  -> (String, InstructionMode, Integer)
  -> [N.Node]
makeInstruction
  opcodeNode
  readByteNode
  readImmediateNode
  readWordNode
  readRelLabelNode
  readAbsLabelNode
  prev
  (name, im, opcode)
  | im == Immediate = prev ++ [opcodeNode name opcode [readImmediateNode]]
  | im == ZeroPage = prev ++ [opcodeNode name opcode [readByteNode]]
  | otherwise = prev ++ [N.defaultNode]

makeInstructions :: [(String, InstructionMode, Integer)] -> [N.Node]
makeInstructions = foldl
  (makeInstruction
     opcodeNode
     readByteNode
     readImmediateNode
     readWordNode
     readRelLabelNode
     readAbsLabelNode)
  []

makeStore name opcode = [ (name, Immediate, mask Immediate opcode)
                        , (name, ZeroPage, mask ZeroPage opcode)]

nodes = makeInstructions $ makeStore "lda" 0xA9
