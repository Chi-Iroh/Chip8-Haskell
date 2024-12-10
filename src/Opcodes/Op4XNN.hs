module Opcodes.Op4XNN (execOp4XNN) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op4XNN), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp4XNN :: OpcodeCallback
execOp4XNN interpreter (Op4XNN args) = Expected interpreter { cpu = cpu' { pc = if vx /= (nn args) then pc' + 2 else pc' } }
    where cpu' = cpu interpreter
          pc' = pc cpu'
          vx = (v cpu') !! (int $ x args)
execOp4XNN _ op = wrongOpcode "4XNN" op