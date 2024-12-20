module Opcodes.OpEXA1 (execOpEXA1) where

import Data.Bits ((.&.))
import Debug.Trace (traceShowId, traceShow)

import CPU (CPU(..))
import Expected (Expected(..), isUnexpected, expected)
import Hex (showHex8)
import Keyboard (nthChip8Key, isKeyPressed)
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpEXA1), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpEXA1 :: OpcodeCallback
execOpEXA1 interpreter (OpEXA1 args)
    | isUnexpected vxToKey = traceShow "exa1b" $ Unexpected $ "V[X] doesn't contain a valid key ! Chip8 keys are in 0x00-0x0F, but got " ++ showHex8 key
    | otherwise = traceShow "exa1" $ Expected interpreter { cpu = cpu' { pc = if traceShowId $ isKeyPressed (traceShowId $ expected vxToKey) then pc' else pc' + 2 } }
    where cpu' = cpu interpreter
          v' = v cpu'
          pc' = pc cpu'
          key = v' !! (int $ x args)
          vxToKey = nthChip8Key (key .&. 0x0F)
execOpEXA1 _ op = wrongOpcode "EXA1" op