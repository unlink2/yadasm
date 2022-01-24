module Yadasm.Archs.Arch65C02 where

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
import qualified Yadasm.Archs.Arch6502 as A6502

mask :: InstructionMode -> Integer -> Integer
mask ZeroPageIndirect opcode
  | isCC10 opcode = makeOpcode 0x12 opcode
mask Immediate opcode = makeOpcode 0x89 opcode -- bit
mask ZeroPageX opcode
  | isCC00 opcode = makeOpcode 0x04 opcode
mask Absolute opcode
  | isCC00 opcode = makeOpcode 0x1C opcode
mask JumpAbsoluteX opcode
  | isCC00 opcode = makeOpcode 0x7C opcode
mask AbsoluteX opcode
  | isCC00 opcode = makeOpcode 0x9E opcode
mask ZeroPage opcode
  | isCC00 opcode = makeOpcode 0x14 opcode
mask im opcode = maskEcho im opcode

makeImInfo :: String -> InstructionMode -> Integer -> ImInfo
makeImInfo = makeImInfo' mask

makeZeroPageIndirect :: String -> Integer -> [ImInfo]
makeZeroPageIndirect name opcode = [makeImInfo name ZeroPageIndirect opcode]

makeJumpIndirect :: String -> Integer -> [ImInfo]
makeJumpIndirect name opcode = [makeImInfo name JumpAbsoluteX opcode]

makeBitExt :: String -> Integer -> [ImInfo]
makeBitExt name opcode = [ makeImInfo' maskEcho name ZeroPageX 0x34
                         , makeImInfo' maskEcho name AbsoluteX 0x3C
                         , makeImInfo' maskEcho name Immediate 0x89]

makeTrb :: String -> Integer -> [ImInfo]
makeTrb name opcode =
  [makeImInfo name ZeroPage opcode, makeImInfo name Absolute opcode]

makeTsb :: String -> Integer -> [ImInfo]
makeTsb name opcode = [ makeImInfo' maskEcho name ZeroPage 0x04
                      , makeImInfo' maskEcho name Absolute 0x0C]

makeStz :: String -> Integer -> [ImInfo]
makeStz name opcode = [ makeImInfo' maskEcho name ZeroPageX 0x74
                      , makeImInfo' maskEcho name AbsoluteX 0x9E
                      , makeImInfo' maskEcho name ZeroPage 0x64
                      , makeImInfo' maskEcho name Absolute 0x9C]

makeAcc :: String -> Integer -> [ImInfo]
makeAcc name opcode = [makeImInfo name Accumulator opcode]

nodes' :: ([ImInfo] -> [N.Node]) -> [N.Node]
nodes' makeInstructions = A6502.nodes' makeInstructions
  ++ makeInstructions
    (makeZeroPageIndirect "ora" 0x12
     ++ makeZeroPageIndirect "and" 0x32
     ++ makeZeroPageIndirect "eor" 0x52
     ++ makeZeroPageIndirect "adc" 0x72
     ++ makeZeroPageIndirect "sta" 0x92
     ++ makeZeroPageIndirect "lda" 0xB2
     ++ makeZeroPageIndirect "cmp" 0xD2
     ++ makeZeroPageIndirect "sbc" 0xF2
     ++ makeJumpIndirect "jmp" 0x7C
     ++ makeBitExt "bit" 0x00
     ++ makeTsb "tsb" 0x00
     ++ makeTrb "trb" 0x1C
     ++ makeStz "stz" 0x74
     ++ A6502.makeBranch "bra" 0x80
     ++ makeAcc "inc" 0x1A
     ++ makeAcc "dec" 0x3A
     ++ A6502.makeImplied "phy" 0x5A
     ++ A6502.makeImplied "ply" 0x7A
     ++ A6502.makeImplied "phx" 0xDA
     ++ A6502.makeImplied "plx" 0xFA)

nodes :: [N.Node]
nodes = nodes' makeInstructions
