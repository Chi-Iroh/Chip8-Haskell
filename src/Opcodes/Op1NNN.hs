module Opcodes.Op1NNN (execOp1NNN) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op1NNN), OpcodeArgs(..), OpcodeCallback)

execOp1NNN :: OpcodeCallback
execOp1NNN interpreter (Op1NNN args) = checkPc (nnn' - 2) >>= (\beforePc -> Expected interpreter { cpu = cpu' { pc = beforePc } })
    where cpu' = cpu interpreter
          nnn' = nnn args
execOp1NNN _ op = wrongOpcode "1NNN" op