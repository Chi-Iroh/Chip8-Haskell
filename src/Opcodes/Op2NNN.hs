module Opcodes.Op2NNN (execOp2NNN) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op2NNN), OpcodeArgs(..), OpcodeCallback)

execOp2NNN :: OpcodeCallback
execOp2NNN interpreter (Op2NNN args) = checkPc (nnn' - 2) >>= (\beforeSubroutine -> Expected interpreter { cpu = cpu' { jumps = (pc cpu') : jumps', pc = beforeSubroutine } })
    where cpu' = cpu interpreter
          nnn' = nnn args
          jumps' = jumps cpu'
execOp2NNN _ op = wrongOpcode "2NNN" op