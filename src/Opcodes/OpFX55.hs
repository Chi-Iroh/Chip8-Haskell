module Opcodes.OpFX55 (execOpFX55) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import List (replaceAt, slice)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpFX55), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpFX55 :: OpcodeCallback
execOpFX55 interpreter (OpFX55 args) = Expected interpreter { cpu = cpu' { memory = replaceAt (int $ i cpu') v0toVX memory' } }
    where cpu' = cpu interpreter
          memory' = memory cpu'
          v0toVX = take (succ $ int $ x args) (v cpu')
execOpFX55 _ op = wrongOpcode "FX55" op