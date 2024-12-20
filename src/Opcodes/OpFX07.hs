module Opcodes.OpFX07 (execOpFX07) where

import CPU (CPU(..))
import Expected (Expected(..))
import Keyboard (waitForChip8Key, chip8KeyValue)
import Interpreter (Interpreter(..))
import List (setAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpFX07), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpFX07 :: OpcodeCallback
execOpFX07 interpreter (OpFX07 args) = Expected interpreter { cpu = cpu' { v = setAt (int $ x args) (sysCounter cpu') v' } }
    where cpu' = cpu interpreter
          v' = v cpu'
execOpFX07 _ op = wrongOpcode "FX07" op