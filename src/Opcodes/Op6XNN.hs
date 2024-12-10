module Opcodes.Op6XNN (execOp6XNN) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import List (setAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op6XNN), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp6XNN :: OpcodeCallback
execOp6XNN interpreter (Op6XNN args) = Expected interpreter { cpu = cpu' { v = setAt (int $ x args) (nn args) (v cpu') } }
    where cpu' = cpu interpreter
execOp6XNN _ op = wrongOpcode "6XNN" op