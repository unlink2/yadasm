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
  | ZeroPageY
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
  | IndirectJump
  deriving (Eq, Show)

data ImInfo = ImInfo String InstructionMode Integer

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

appendStringNode :: String -> N.Node
appendStringNode text = N.defaultNode { N.converter = textConverter text }

opcodeNode :: String -> Integer -> [N.Node] -> N.Node
opcodeNode name opcode children =
  N.defaultNode { N.reader = B.read1le
                , N.converter = textConverter (name ++ " ")
                , N.comparator = opcodeComparator opcode
                , N.children = children
                , N.size = 1
                }

readByteNode :: N.Node
readByteNode = N.defaultNode { N.reader = B.read1le
                             , N.converter = numConverter "$%02X"
                             , N.size = 1
                             }

readImmediateNode :: N.Node
readImmediateNode =
  readByteNode { N.converter = numConverter "#$%02X", N.size = 1 }

readWordNode :: N.Node
readWordNode = N.defaultNode { N.reader = B.read2le
                             , N.converter = numConverter "$%04X"
                             , N.size = 2
                             }

readRelLabelNode :: N.Node
readRelLabelNode = N.defaultNode

readAbsLabelNode :: N.Node
readAbsLabelNode = N.defaultNode

-- take an opcode and extract the CC bits 
extractCCBits :: Integer -> Integer
extractCCBits opcode = opcode .&. 0x03 -- 0b00000011

-- mask out unwanted parts of an opcode to apply an addressing mode
applyMask :: Integer -> Integer
applyMask opcode = opcode .&. 0x1C -- 0b00011100

-- extract remainder of opcode without addressing mode 
-- information 
extractOpcode :: Integer -> Integer
extractOpcode opcode = opcode .&. 0xE3 -- 0b11100011 

isCC01 :: Integer -> Bool
isCC01 opcode = extractCCBits opcode == 0x01

isCC10 :: Integer -> Bool
isCC10 opcode = extractCCBits opcode == 0x02

isCC00 :: Integer -> Bool
isCC00 opcode = extractCCBits opcode == 0x00

makeOpcode :: Integer -> Integer -> Integer
makeOpcode base opcode = applyMask base .|. extractOpcode opcode

-- turns an opcode into the desired addressing mode 
-- depending on the cc bits
mask :: InstructionMode -> Integer -> Integer
mask Immediate opcode
  | isCC00 opcode = makeOpcode 0xE0 opcode
  | isCC01 opcode = makeOpcode 0x69 opcode
  | isCC10 opcode = makeOpcode 0xA2 opcode
mask ZeroPage opcode
  | isCC00 opcode = makeOpcode 0xE4 opcode
  | isCC01 opcode = makeOpcode 0x65 opcode
  | isCC10 opcode = makeOpcode 0xA6 opcode
mask ZeroPageX opcode
  | isCC00 opcode = makeOpcode 0xB4 opcode
  | isCC01 opcode = makeOpcode 0x75 opcode
  | isCC10 opcode = makeOpcode 0x75 opcode
mask ZeroPageY opcode
  | isCC10 opcode = makeOpcode 0xB6 opcode
mask Absolute opcode
  | isCC00 opcode = makeOpcode 0xEC opcode
  | isCC01 opcode = makeOpcode 0x6D opcode
  | isCC10 opcode = makeOpcode 0xAE opcode
mask AbsoluteJump opcode
  | isCC00 opcode = makeOpcode 0xEC opcode
mask AbsoluteX opcode
  | isCC00 opcode = makeOpcode 0xBC opcode
  | isCC01 opcode = makeOpcode 0x7D opcode
  | isCC10 opcode = makeOpcode 0x7D opcode
mask AbsoluteY opcode
  | isCC01 opcode = makeOpcode 0x79 opcode
  | isCC10 opcode = makeOpcode 0xBE opcode
mask IndirectX opcode
  | isCC01 opcode = makeOpcode 0x61 opcode
mask IndirectY opcode
  | isCC01 opcode = makeOpcode 0x71 opcode
mask Implied opcode
  | isCC01 opcode = makeOpcode 0x00 opcode
mask Relative opcode
  | isCC00 opcode = makeOpcode 0x00 opcode
  | isCC01 opcode = makeOpcode 0x10 opcode
mask Accumulator opcode
  | isCC10 opcode = makeOpcode 0x0A opcode
mask IndirectJump opcode
  | isCC00 opcode = 0x6C -- indirect jmp breaks rules! 
mask Implied opcode = opcode
mask _ opcode = opcode

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
  -> ImInfo
  -> [N.Node]
makeInstruction
  opcodeNode
  readByteNode
  readImmediateNode
  readWordNode
  readRelLabelNode
  readAbsLabelNode
  prev
  (ImInfo name im opcode)
  | im == Immediate = prev ++ [opcodeNode name opcode [readImmediateNode]]
  | im == ZeroPage = prev ++ [opcodeNode name opcode [readByteNode]]
  | im == ZeroPageX =
    prev ++ [opcodeNode name opcode [readByteNode, appendStringNode ", x"]]
  | im == Absolute = prev ++ [opcodeNode name opcode [readWordNode]]
  | im == AbsoluteX =
    prev ++ [opcodeNode name opcode [readWordNode, appendStringNode ", x"]]
  | im == AbsoluteY =
    prev ++ [opcodeNode name opcode [readWordNode, appendStringNode ", y"]]
  | im == IndirectX = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "(", readByteNode, appendStringNode ", x)"]]
  | im == IndirectY = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "(", readByteNode, appendStringNode "), y"]]
  | otherwise = prev ++ [N.defaultNode { N.comparator = Cmp.alwaysFalse }]

makeInstructions :: [ImInfo] -> [N.Node]
makeInstructions = foldl
  (makeInstruction
     opcodeNode
     readByteNode
     readImmediateNode
     readWordNode
     readRelLabelNode
     readAbsLabelNode)
  []

makeImInfo :: String -> InstructionMode -> Integer -> ImInfo
makeImInfo name im opcode = ImInfo name im (mask im opcode)

makeStore :: String -> Integer -> [ImInfo]
makeStore name opcode =
  [ makeImInfo name ZeroPage opcode
  , makeImInfo name ZeroPageX opcode
  , makeImInfo name Absolute opcode
  , makeImInfo name AbsoluteX opcode
  , makeImInfo name AbsoluteY opcode
  , makeImInfo name IndirectX opcode
  , makeImInfo name IndirectY opcode]

makeLoad :: String -> Integer -> [ImInfo]
makeLoad name opcode = makeImInfo name Immediate opcode:makeStore name opcode

nodes :: [N.Node]
nodes = makeInstructions $ makeLoad "lda" 0xA9
