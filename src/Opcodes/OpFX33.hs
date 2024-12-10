module Opcodes.OpFX33 (execOpFX33) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import List (replaceAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpFX33), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpFX33 :: OpcodeCallback
execOpFX33 interpreter (OpFX33 args) = Expected interpreter { cpu = cpu' { memory = bcd } }
    where cpu' = cpu interpreter
          pc' = pc cpu'
          vx = (v cpu') !! (int $ x args)
          bcd = replaceAt (int $ i cpu') [div vx 100, rem (div vx 10) 10, rem vx 10] (memory cpu')
execOpFX33 _ op = wrongOpcode "FX33" op