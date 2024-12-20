module Opcodes.OpBNNN (execOpBNNN) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpBNNN), OpcodeArgs(..), OpcodeCallback)
import Word (u8to16)

execOpBNNN :: OpcodeCallback
execOpBNNN interpreter (OpBNNN args) = Expected interpreter { cpu = cpu' { pc = (nnn args) + u8to16 (head v') - 2 } }
    where cpu' = cpu interpreter
          v' = v cpu'
execOpBNNN _ op = wrongOpcode "BNNN" op