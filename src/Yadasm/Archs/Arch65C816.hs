module Yadasm.Archs.Arch65C816 where

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
import qualified Yadasm.Archs.Arch65C02 as A65C02

mask :: InstructionMode -> Integer -> Integer
mask StackS opcode
  | isCC11 opcode = makeOpcode 0x03 opcode
mask DirectPageIndirectLong opcode
  | isCC11 opcode = makeOpcode 0x07 opcode
mask AbsoluteLong opcode
  | isCC11 opcode = makeOpcode 0x0F opcode
mask StackSY opcode
  | isCC11 opcode = makeOpcode 0x13 opcode
mask DirectPageIndirectLongY opcode
  | isCC11 opcode = makeOpcode 0x17 opcode
mask AbsoluteLongX opcode
  | isCC11 opcode = makeOpcode 0x1F opcode
mask im opcode = maskEcho im opcode

makeImInfo :: String -> InstructionMode -> Integer -> ImInfo
makeImInfo = makeImInfo' mask

makeExtended :: String -> Integer -> [ImInfo]
makeExtended name opcode =
  [ makeImInfo name AbsoluteLong opcode
  , makeImInfo name AbsoluteLongX opcode
  , makeImInfo name DirectPageIndirectLong opcode
  , makeImInfo name DirectPageIndirectLongY opcode
  , makeImInfo name StackS opcode
  , makeImInfo name StackSY opcode]

makeCop :: String -> Integer -> [ImInfo]
makeCop name opcode = [makeImInfo name ImmediateByte opcode]

makeJumpLong :: String -> Integer -> [ImInfo]
makeJumpLong name opcode = [makeImInfo name JumpAbsoluteLong opcode]

makePer :: String -> Integer -> [ImInfo]
makePer name opcode = [makeImInfo name Per opcode]

makeBrl :: String -> Integer -> [ImInfo]
makeBrl name opcode = [makeImInfo name BranchLong opcode]

makeMove :: String -> Integer -> [ImInfo]
makeMove name opcode = [makeImInfo name Move opcode]

makePei :: String -> Integer -> [ImInfo]
makePei name opcode = [makeImInfo name ZeroPage opcode]

makePea :: String -> Integer -> [ImInfo]
makePea name opcode = [makeImInfo name Immediate opcode]

makeJml :: String -> Integer -> [ImInfo]
makeJml name opcode = [makeImInfo name Jml opcode]

makeJsrx :: String -> Integer -> [ImInfo]
makeJsrx name opcode = [makeImInfo name Jsrx opcode]

nodes' :: ([ImInfo] -> [N.Node]) -> [N.Node]
nodes'
  makeInstructions = A65C02.nodes' makeInstructions ++ node1 ++ node2 ++ node3
  where
    -- again split up as a workaround to lsp 
    -- having issues with large ++ chains
    node1 = makeInstructions
      (makeExtended "ora" 0x03
       ++ makeExtended "and" 0x23
       ++ makeExtended "eor" 0x43
       ++ makeExtended "adc" 0x63
       ++ makeExtended "sta" 0x83
       ++ makeExtended "lda" 0xA3
       ++ makeExtended "cmp" 0xC3
       ++ makeExtended "sbc" 0xE3
       ++ A6502.makeImplied "xce" 0xFB
       ++ makeCop "cop" 0x02)

    node2 = makeInstructions
      (A6502.makeImplied "phd" 0x0B
       ++ A6502.makeImplied "pld" 0x2B
       ++ A6502.makeImplied "phk" 0x4B
       ++ A6502.makeImplied "rtl" 0x6B
       ++ A6502.makeImplied "phb" 0x8B
       ++ A6502.makeImplied "plb" 0xAB
       ++ A6502.makeImplied "wai" 0xCB
       ++ A6502.makeImplied "xba" 0xEB
       ++ A6502.makeImplied "tcs" 0x1B
       ++ A6502.makeImplied "tsc" 0x3B
       ++ A6502.makeImplied "tcd" 0x5B
       ++ A6502.makeImplied "tdc" 0x7B
       ++ A6502.makeImplied "txy" 0x9B
       ++ A6502.makeImplied "tyx" 0xBB
       ++ A6502.makeImplied "stp" 0xDB
       ++ A6502.makeImplied "wdm" 0x42)

    node3 = makeInstructions
      (makeJumpLong "jsl" 0x22
       ++ makePer "per" 0x62
       ++ makeBrl "brl" 0x82
       ++ makeCop "rep" 0xC2
       ++ makeCop "sep" 0xE2
       ++ makeMove "mvp" 0x44
       ++ makeMove "mvn" 0x54
       ++ makePei "pei" 0xD4
       ++ makePea "pea" 0xF4
       ++ makeJumpLong "jmp" 0x5C
       ++ makeJml "jmp" 0xDC
       ++ makeJsrx "jsr" 0xFC)

nodes :: [N.Node]
nodes = nodes' (makeInstructions' 2)

nodesEmulated :: [N.Node]
nodesEmulated = nodes' (makeInstructions' 1)
