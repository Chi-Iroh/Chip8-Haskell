module Opcodes.OpFX15 (execOpFX15) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import List (setAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpFX15), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpFX15 :: OpcodeCallback
execOpFX15 interpreter (OpFX15 args) = Expected interpreter { cpu = cpu' { sysCounter = v' !! (int $ x args) } }
    where cpu' = cpu interpreter
          v' = v cpu'
execOpFX15 _ op = wrongOpcode "FX15" op