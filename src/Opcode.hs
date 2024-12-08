module Opcode (identifyOpcode) where

import Data.Bits
import Data.List (find)
import Data.Maybe (fromJust, isJust)

import Bits
import CPU (CPU(..), memorySize, memoryStart)
import Expected (Expected(..))
import OpcodeTypes
import Word

inRange :: Ord a => a -> a -> a -> Bool
inRange start end a = start <= a && a <= end

read8 :: [Word8] -> Word16 -> Word8
read8 mem = (mem !!) . int

rawOpcode' :: [Word8] -> Word16 -> Word16
rawOpcode' memory' pc' = shiftL b1 8 .&. b2
    where [b1, b2] = map u8to16 [read8 memory' pc', read8 memory' (pc' + 1)]

rawOpcode :: CPU -> Expected Word16
rawOpcode cpu
    | not (inRange memoryStart (memorySize - 2) pc') = Unexpected ("Out of range PC ! Got " ++ show pc' ++ " but needs to be in (" ++ show memoryStart ++ ", " ++ show (memorySize - 2) ++ ") !")
    | otherwise = Expected (rawOpcode' memory' pc')
        where memory' = memory cpu
              pc' = pc cpu



exactOpcodes :: [(Word16, Opcode)]
exactOpcodes = [(0x00E0, Op00E0), (0x00EE, Op00EE)]

type Mask = Word16
type MaskResult = Word16

maskedOpcodes :: [(Mask, MaskResult, OpcodeArgs -> Opcode)]
maskedOpcodes = [   (0xF000, 0x0000, Op0NNN)
                ,   (0xF000, 0x1000, Op1NNN)
                ,   (0xF000, 0x2000, Op2NNN)
                ,   (0xF000, 0x3000, Op3XNN)
                ,   (0xF000, 0x4000, Op4XNN)
                ,   (0xF00F, 0x5000, Op5XY0)
                ,   (0xF000, 0x6000, Op6XNN)
                ,   (0xF000, 0x7000, Op7XNN)
                ,   (0xF00F, 0x8000, Op8XY0)
                ,   (0xF00F, 0x8001, Op8XY1)
                ,   (0xF00F, 0x8002, Op8XY2)
                ,   (0xF00F, 0x8003, Op8XY3)
                ,   (0xF00F, 0x8004, Op8XY4)
                ,   (0xF00F, 0x8005, Op8XY5)
                ,   (0xF00F, 0x8006, Op8XY6)
                ,   (0xF00F, 0x8007, Op8XY7)
                ,   (0xF00F, 0x800E, Op8XYE)
                ,   (0xF00F, 0x9000, Op9XY0)
                ,   (0xF000, 0xA000, OpANNN)
                ,   (0xF000, 0xB000, OpBNNN)
                ,   (0xF000, 0xC000, OpCXNN)
                ,   (0xF000, 0xD000, OpDXYN)
                ,   (0xF0FF, 0xE09E, OpEX9E)
                ,   (0xF0FF, 0xE0A1, OpEXA1)
                ,   (0xF0FF, 0xF007, OpFX07)
                ,   (0xF0FF, 0xF015, OpFX15)
                ,   (0xF0FF, 0xF018, OpFX18)
                ,   (0xF0FF, 0xF01E, OpFX1E)
                ,   (0xF0FF, 0xF029, OpFX29)
                ,   (0xF0FF, 0xF033, OpFX33)
                ,   (0xF0FF, 0xF055, OpFX55)
                ,   (0xF0FF, 0xF065, OpFX65) ]

thd :: (a, b, c) -> c
thd (_, _, c) = c

makeOpcodeArgs :: Word16 -> OpcodeArgs
makeOpcodeArgs op = OpcodeArgs {
    x = u16to8 (op .&. 0x0F00 .>> 8),
    y = u16to8 (op .&. 0x00F0 .>> 4),
    n = u16to8 (op .&. 0x000F),
    nn = u16to8 (op .&. 0x00FF),
    nnn = op .&. 0x0FFF
}

identifyOpcode :: Word16 -> Expected Opcode
identifyOpcode op
    | isJust exactOp = Expected (snd $ fromJust exactOp)
    | isJust maskedOp = Expected ((thd $ fromJust maskedOp) (makeOpcodeArgs op))
    | otherwise = Unexpected "Cannot identify opcode "
    where exactOp = find ((== op) . fst) exactOpcodes
          maskedOp = find (\(mask, res, _) -> mask .&. op == res) maskedOpcodes

readOpcode :: CPU -> Expected Opcode
readOpcode cpu = Unexpected "d"
    where raw = rawOpcode cpu
