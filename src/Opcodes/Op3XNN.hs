module Opcodes.Op3XNN (execOp3XNN) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op3XNN), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp3XNN :: OpcodeCallback
execOp3XNN interpreter (Op3XNN args) = Expected interpreter { cpu = cpu' { pc = if vx == (nn args) then pc' + 2 else pc' } }
    where cpu' = cpu interpreter
          pc' = pc cpu'
          vx = (v cpu') !! (int $ x args)
execOp3XNN _ op = wrongOpcode "3XNN" op