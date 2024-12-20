module Opcodes.OpFX18 (execOpFX18) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpFX18), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpFX18 :: OpcodeCallback
execOpFX18 interpreter (OpFX18 args) = Expected interpreter { cpu = cpu' { soundCounter = v' !! (int $ x args) } }
    where cpu' = cpu interpreter
          v' = v cpu'
execOpFX18 _ op = wrongOpcode "FX18" op