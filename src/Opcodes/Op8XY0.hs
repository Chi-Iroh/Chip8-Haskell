module Opcodes.Op8XY0 (execOp8XY0) where

import Data.Bits ((.|.))

import CPU (CPU(..))
import Expected (Expected(..))
import Interpreter
import List (setAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op8XY0), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp8XY0 :: OpcodeCallback
execOp8XY0 interpreter (Op8XY0 args) = Expected interpreter { cpu = cpu' { v = setAt (int $ x args) vy (v cpu') } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vy = v' !! (int $ y args)
execOp8XY0 _ op = wrongOpcode "8XY0" op