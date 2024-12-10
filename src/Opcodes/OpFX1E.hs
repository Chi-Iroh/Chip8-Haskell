module Opcodes.OpFX1E (execOpFX1E) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpFX1E), OpcodeArgs(..), OpcodeCallback)
import Word (int, u8to16)

execOpFX1E :: OpcodeCallback
execOpFX1E interpreter (OpFX1E args) = Expected interpreter { cpu = cpu' { i = i' + (u8to16 $ v' !! (int $ x args)) } }
    where cpu' = cpu interpreter
          i' = i cpu'
          v' = v cpu'
execOpFX1E _ op = wrongOpcode "FX1E" op