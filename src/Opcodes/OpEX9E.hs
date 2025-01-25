module Opcodes.OpEX9E (execOpEX9E) where

import Data.Bits ((.&.))

import CPU (CPU(..))
import Expected (Expected(..), isUnexpected, expected)
import Hex (showHex8)
import Keyboard (nthChip8Key, isKeyPressed)
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpEX9E), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpEX9E :: OpcodeCallback
execOpEX9E interpreter (OpEX9E args)
    | isUnexpected vxToKey = Unexpected $ "V[X] doesn't contain a valid key ! Chip8 keys are in 0x00-0x0F, but got " ++ showHex8 key
    | otherwise = Expected interpreter { cpu = cpu' { pc = if isKeyPressed (expected vxToKey) then pc' + 2 else pc' } }
    where cpu' = cpu interpreter
          v' = v cpu'
          pc' = pc cpu'
          key = v' !! (int $ x args)
          vxToKey = nthChip8Key (key .&. 0x0F)
execOpEX9E _ op = wrongOpcode "EX9E" op