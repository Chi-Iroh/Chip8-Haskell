module Opcodes.OpFX65 (execOpFX65) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import List (replaceAt, slice)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpFX65), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpFX65 :: OpcodeCallback
execOpFX65 interpreter (OpFX65 args) = Expected interpreter { cpu = cpu' { v = replaceAt 0 v0toVX v' } }
    where cpu' = cpu interpreter
          v' = v cpu'
          i' = int $ i cpu'
          v0toVX = slice i' (i' + 1 + (int $ x args)) (memory cpu')
execOpFX65 _ op = wrongOpcode "FX65" op