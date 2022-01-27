module Yadasm.Archs.Arch6502Helpers where

import           Data.Bits
import qualified Yadasm.Node as N
import qualified Yadasm.Binary as B
import qualified Data.ByteString as ByteString
import qualified Yadasm.Symbol as S
import qualified Yadasm.Line as L
import qualified Yadasm.Context as C
import qualified Yadasm.Comparator as Cmp
import           Text.Printf
import           Data.Maybe (isNothing, isJust)
import           Numeric (showHex)
import qualified Yadasm.Definition as D

data InstructionMode =
    Immediate -- dynamically sized 
  | ImmediateByte -- always 1 byte
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
  | ZeroPageIndirect
  | JumpAbsoluteX
  | StackS
  | DirectPageIndirectLong
  | AbsoluteLong
  | StackSY
  | DirectPageIndirectLongY
  | AbsoluteLongX
  | JumpAbsoluteLong
  | Per
  | BranchLong
  | Move
  | Jml
  | Jsrx
  deriving (Eq, Show)

data ImInfo = ImInfo String InstructionMode Integer
  deriving (Eq, Show)

-- simply returns the node's name as a codeword
textConverter
  :: String -> C.Context -> Integer -> Int -> Maybe ([L.CodeWord], [S.Symbol])
textConverter text ctx dat size =
  Just ([L.defaultCodeWord { L.text = text, L.size = size, L.raw = dat }], [])

numConverter :: String
             -> String
             -> String
             -> L.Attrs
             -> C.Context
             -> Integer
             -> Int
             -> Maybe ([L.CodeWord], [S.Symbol])
numConverter prefix fmt postfix attr ctx dat size = retDef def
  where
    def = C.lookupDefinition dat ctx

    retDef (Just def) = Just
      ( [ L.defaultCodeWord { L.text =
                                prefix ++ printf "%s" (D.text def) ++ postfix
                            , L.size = size
                            , L.raw = dat
                            , L.attr = attr
                            }]
      , [])
    retDef Nothing = Just
      ( [ L.defaultCodeWord { L.text = prefix ++ printf fmt dat ++ postfix
                            , L.size = size
                            , L.raw = dat
                            , L.attr = attr
                            }]
      , [])

symbolToResult Nothing size = Nothing
symbolToResult (Just []) size = Nothing
symbolToResult (Just (sym:syms)) size =
  Just ([L.defaultCodeWord { L.text = S.name sym, L.size = size }], [])

relAddrConverter :: C.Context -> Integer -> Integer
relAddrConverter ctx dat
  | dat .&. 0x80 > 0 = C.address ctx - (complement dat + 0xFF)
  | otherwise = C.address ctx + dat + 2

relWordAddrConverter :: C.Context -> Integer -> Integer
relWordAddrConverter ctx dat
  | dat .&. 0x8000 > 0 = C.address ctx - (complement dat + 0xFFFF)
  | otherwise = C.address ctx + dat + 2

absAddrConverter :: C.Context -> Integer -> Integer
absAddrConverter ctx dat = dat

labelConverter :: (C.Context -> Integer -> Integer)
               -> C.Context
               -> Integer
               -> Int
               -> Maybe ([L.CodeWord], [S.Symbol])
labelConverter addr ctx dat size
  | isNothing sym = Just
    ( [ L.defaultCodeWord { L.text = buildSymText (addr ctx dat)
                          , L.size = size
                          , L.raw = dat
                          }]
    , [ S.defaultSymbol { S.name = buildSymText (addr ctx dat)
                        , S.address = addr ctx dat
                        }])
  | otherwise = symbolToResult sym size
  where
    sym = C.getSymbolAt ctx (addr ctx dat)

    buildSymText addr
      | addr < 0 = printf "label_mi_%X" (abs addr)
    buildSymText addr = printf "label_%X" addr

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

opcodeNodeNoSpace :: String -> Integer -> [N.Node] -> N.Node
opcodeNodeNoSpace name opcode children =
  (opcodeNode name opcode children) { N.converter = textConverter name }

readByteNode' :: String -> String -> L.Attrs -> [N.Node] -> N.Node
readByteNode' prefix postfix attr children =
  N.defaultNode { N.reader = B.read1le
                , N.converter = numConverter prefix "$%02X" postfix attr
                , N.size = 1
                , N.children = children
                }

readByteNode :: N.Node
readByteNode = readByteNode' "" "" L.Std []

readImmediateNode :: Int -> N.Node
readImmediateNode 1 =
  readByteNode { N.converter = numConverter "#" "$%02X" "" L.Std, N.size = 1 }
readImmediateNode _ =
  readWordNode { N.converter = numConverter "#" "$%04X" "" L.Std, N.size = 2 }

readWordNode' :: String -> String -> L.Attrs -> [N.Node] -> N.Node
readWordNode' prefix postfix attr children =
  N.defaultNode { N.reader = B.read2le
                , N.converter = numConverter prefix "$%04X" postfix attr
                , N.size = 2
                , N.children = children
                }

readWordNode :: N.Node
readWordNode = readWordNode' "" "" L.Std []

readLWordNode' :: String -> String -> L.Attrs -> [N.Node] -> N.Node
readLWordNode' prefix postfix attr children =
  N.defaultNode { N.reader = B.read3le
                , N.converter = numConverter prefix "$%06X" postfix attr
                , N.size = 3
                , N.children = children
                }

readLWordNode :: N.Node
readLWordNode = readLWordNode' "" "" L.Std []

readRelLabelNode :: N.Node
readRelLabelNode =
  N.defaultNode { N.reader = B.read1le
                , N.converter = labelConverter relAddrConverter
                , N.size = 1
                }

readRelWordLabelNode :: N.Node
readRelWordLabelNode =
  N.defaultNode { N.reader = B.read2le
                , N.converter = labelConverter relWordAddrConverter
                , N.size = 2
                }

readAbsLabelNode :: N.Node
readAbsLabelNode =
  N.defaultNode { N.reader = B.read2le
                , N.converter = labelConverter absAddrConverter
                , N.size = 2
                }

readAbsLongLabelNode :: N.Node
readAbsLongLabelNode =
  N.defaultNode { N.reader = B.read3le
                , N.converter = labelConverter absAddrConverter
                , N.size = 3
                }

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

isCC11 :: Integer -> Bool
isCC11 opcode = extractCCBits opcode == 0x03

makeOpcode :: Integer -> Integer -> Integer
makeOpcode base opcode = applyMask base .|. extractOpcode opcode

-- alternative maks implementation 
-- for opcodes that break the rules and just 
-- need to be echoed
maskEcho :: InstructionMode -> Integer -> Integer
maskEcho im opcode = opcode

-- create an instruction from the input functions 
-- and the supplied data 
makeInstruction
  :: (String -> Integer -> [N.Node] -> N.Node)
  -> N.Node
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
  readLWordNode
  readRelLabelNode
  readAbsLabelNode
  prev
  (ImInfo name im opcode)
  | im == Immediate = prev ++ [opcodeNode name opcode [readImmediateNode]]
  | im == ImmediateByte = prev
    ++ [ opcodeNode
           name
           opcode
           [Yadasm.Archs.Arch6502Helpers.readImmediateNode 1]]
  | im == Relative = prev ++ [opcodeNode name opcode [readRelLabelNode]]
  | im == Implied = prev ++ [opcodeNodeNoSpace name opcode []]
  | im == ZeroPage = prev ++ [opcodeNode name opcode [readByteNode]]
  | im == ZeroPageX =
    prev ++ [opcodeNode name opcode [readByteNode, appendStringNode ", x"]]
  | im == ZeroPageY =
    prev ++ [opcodeNode name opcode [readByteNode, appendStringNode ", y"]]
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
  | im == Accumulator = prev ++ [opcodeNode name opcode [appendStringNode "A"]]
  | im == AbsoluteJump = prev ++ [opcodeNode name opcode [readAbsLabelNode]]
  | im == IndirectJump = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "(", readAbsLabelNode, appendStringNode ")"]]
  | im == ZeroPageIndirect = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "(", readByteNode, appendStringNode ")"]]
  | im == JumpAbsoluteX = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "(", readAbsLabelNode, appendStringNode ", x)"]]
  | im == AbsoluteLong = prev ++ [opcodeNode name opcode [readLWordNode]]
  | im == AbsoluteLongX =
    prev ++ [opcodeNode name opcode [readLWordNode, appendStringNode ", x"]]
  | im == DirectPageIndirectLong = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "[", readByteNode, appendStringNode "]"]]
  | im == DirectPageIndirectLongY = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "[", readByteNode, appendStringNode "], y"]]
  | im == StackS =
    prev ++ [opcodeNode name opcode [readByteNode, appendStringNode ", s"]]
  | im == StackSY = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "(", readByteNode, appendStringNode ", s), y"]]
  | im == JumpAbsoluteLong =
    prev ++ [opcodeNode name opcode [readAbsLongLabelNode]]
  | im == Per = prev ++ [opcodeNode name opcode [readAbsLabelNode]]
  | im == BranchLong = prev ++ [opcodeNode name opcode [readRelWordLabelNode]]
  | im == Move = prev
    ++ [ opcodeNode
           name
           opcode
           [ Yadasm.Archs.Arch6502Helpers.readImmediateNode 1
           , appendStringNode ", "
           , Yadasm.Archs.Arch6502Helpers.readImmediateNode 1]]
  | im == Jml = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "[", readAbsLabelNode, appendStringNode "]"]]
  | im == Jsrx = prev
    ++ [ opcodeNode
           name
           opcode
           [appendStringNode "(", readAbsLabelNode, appendStringNode ", x)"]]
  | otherwise = prev ++ [N.defaultNode { N.comparator = Cmp.alwaysFalse }]

makeInstructions' :: Int -> [ImInfo] -> [N.Node]
makeInstructions' size = foldl
  (makeInstruction
     opcodeNode
     readByteNode
     (readImmediateNode size)
     readWordNode
     readLWordNode
     readRelLabelNode
     readAbsLabelNode)
  []

makeInstructions :: [ImInfo] -> [N.Node]
makeInstructions = makeInstructions' 1

makeImInfo' :: (InstructionMode -> Integer -> Integer)
            -> String
            -> InstructionMode
            -> Integer
            -> ImInfo
makeImInfo' mask name im opcode = ImInfo name im (mask im opcode)

defaultNode :: N.Node
defaultNode =
  N.defaultNode { N.reader = B.read1le
                , N.converter = numConverter "!byte " "$%02X" "" L.Std
                , N.size = 1
                }
