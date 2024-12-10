module Opcodes.Op7XNN (execOp7XNN) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import MapUtils (mapAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op7XNN), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp7XNN :: OpcodeCallback
execOp7XNN interpreter (Op7XNN args) = Expected interpreter { cpu = cpu' { v = mapAt (+ (nn args)) (int $ x args) (v cpu') } }
    where cpu' = cpu interpreter
execOp7XNN _ op = wrongOpcode "7XNN" op