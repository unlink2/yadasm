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
import           Data.Maybe (isNothing)
import           Yadasm.Archs.Arch6502Helpers

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
  | isCC00 opcode = opcode
  | isCC01 opcode = opcode
mask Accumulator opcode
  | isCC10 opcode = makeOpcode 0x0A opcode
mask IndirectJump opcode
  | isCC00 opcode = 0x6C -- indirect jmp breaks rules! 
mask Implied opcode = opcode
mask im opcode = maskEcho im opcode

makeImInfo :: String -> InstructionMode -> Integer -> ImInfo
makeImInfo = makeImInfo' mask

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

makeAcc :: String -> Integer -> [ImInfo]
makeAcc name opcode = [makeImInfo name Accumulator opcode]

makeLogic :: String -> Integer -> [ImInfo]
makeLogic name opcode = makeAcc name opcode
  ++ [ makeImInfo name ZeroPage opcode
     , makeImInfo name ZeroPageX opcode
     , makeImInfo name Absolute opcode
     , makeImInfo name AbsoluteX opcode]

makeBit :: String -> Integer -> [ImInfo]
makeBit name opcode =
  [makeImInfo name ZeroPage opcode, makeImInfo name Absolute opcode]

makeBranch :: String -> Integer -> [ImInfo]
makeBranch name opcode = [makeImInfo name Relative opcode]

makeImplied :: String -> Integer -> [ImInfo]
makeImplied name opcode = [makeImInfo name Implied opcode]

makeCompareIndex :: String -> Integer -> [ImInfo]
makeCompareIndex name opcode = [ makeImInfo name Immediate opcode
                               , makeImInfo name ZeroPage opcode
                               , makeImInfo name Absolute opcode]

makeDec :: String -> Integer -> [ImInfo]
makeDec name opcode = [ makeImInfo name ZeroPage opcode
                      , makeImInfo name ZeroPageX opcode
                      , makeImInfo name Absolute opcode
                      , makeImInfo name AbsoluteX opcode]

makeJump :: String -> Integer -> [ImInfo]
makeJump name opcode =
  [makeImInfo name AbsoluteJump opcode, makeImInfo name IndirectJump opcode]

makeJsr :: String -> Integer -> [ImInfo]
makeJsr name opcode = [makeImInfo' maskEcho name AbsoluteJump opcode]

makeLoadX :: String -> Integer -> [ImInfo]
makeLoadX name opcode =
  [ makeImInfo name Immediate opcode
  , makeImInfo name ZeroPage opcode
  , makeImInfo name ZeroPageY opcode
  , makeImInfo name Absolute opcode
  , makeImInfo name AbsoluteY opcode]

makeLoadY :: String -> Integer -> [ImInfo]
makeLoadY name opcode =
  [ makeImInfo name Immediate opcode
  , makeImInfo name ZeroPage opcode
  , makeImInfo name ZeroPageX opcode
  , makeImInfo name Absolute opcode
  , makeImInfo name AbsoluteX opcode]

makeStoreX :: String -> Integer -> [ImInfo]
makeStoreX name opcode = [ makeImInfo name ZeroPage opcode
                         , makeImInfo name ZeroPageY opcode
                         , makeImInfo name Absolute opcode]

makeStoreY :: String -> Integer -> [ImInfo]
makeStoreY name opcode = [ makeImInfo name ZeroPage opcode
                         , makeImInfo name ZeroPageX opcode
                         , makeImInfo name Absolute opcode]

nodes' :: ([ImInfo] -> [N.Node]) -> [N.Node]
nodes' makeInstructions = nodes1 ++ nodes2 ++ nodes3
  where
    -- the following node calls are only separated 
    -- because the haskell lsp server did not like such a long function
    nodes1 :: [N.Node]
    nodes1 = makeInstructions
      (makeLoad "adc" 0x69
       ++ makeLogic "asl" 0x1E
       ++ makeLoad "and" 0x21
       ++ makeBit "bit" 0x24
       ++ makeBranch "bpl" 0x10
       ++ makeBranch "bmi" 0x30
       ++ makeBranch "bvc" 0x50
       ++ makeBranch "bvs" 0x70
       ++ makeBranch "bcc" 0x90
       ++ makeBranch "bcs" 0xB0
       ++ makeBranch "bne" 0xD0
       ++ makeBranch "beq" 0xF0
       ++ makeImplied "brk" 0x00
       ++ makeLoad "cmp" 0xC9
       ++ makeCompareIndex "cpx" 0xE0
       ++ makeCompareIndex "cpy" 0xC0
       ++ makeDec "dec" 0xC6
       ++ makeLoad "eor" 0x49
       ++ makeImplied "clc" 0x18)

    nodes2 :: [N.Node]
    nodes2 = makeInstructions
      (makeImplied "sec" 0x38
       ++ makeImplied "cli" 0x58
       ++ makeImplied "sei" 0x78
       ++ makeImplied "clv" 0xB8
       ++ makeImplied "cld" 0xD8
       ++ makeImplied "sed" 0xF8
       ++ makeDec "inc" 0xE6
       ++ makeJump "jmp" 0x4C
       ++ makeJsr "jsr" 0x20
       ++ makeLoad "lda" 0xA9
       ++ makeLoadX "ldx" 0xA2
       ++ makeLoadY "ldy" 0xA0
       ++ makeLogic "lsr" 0x4A
       ++ makeImplied "nop" 0xEA
       ++ makeLoad "ora" 0x09
       ++ makeImplied "tax" 0xAA
       ++ makeImplied "txa" 0x8A
       ++ makeImplied "dex" 0xCA)

    nodes3 :: [N.Node]
    nodes3 = makeInstructions
      (makeImplied "inx" 0xE8
       ++ makeImplied "tay" 0xA8
       ++ makeImplied "dey" 0x88
       ++ makeImplied "iny" 0xC8
       ++ makeLogic "rol" 0x2A
       ++ makeLogic "ror" 0x6A
       ++ makeImplied "rti" 0x40
       ++ makeImplied "rts" 0x60
       ++ makeLoad "sbc" 0xE9
       ++ makeStore "sta" 0x85
       ++ makeImplied "txs" 0x9A
       ++ makeImplied "tsx" 0xBA
       ++ makeImplied "pha" 0x48
       ++ makeImplied "pla" 0x68
       ++ makeImplied "php" 0x08
       ++ makeImplied "plp" 0x28
       ++ makeStoreX "stx" 0x86
       ++ makeStoreY "sty" 0x84)

nodes :: [N.Node]
nodes = nodes' makeInstructions
