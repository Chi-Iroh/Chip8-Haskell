module Opcodes.Op8XY1 (execOp8XY1) where

import Data.Bits ((.|.))

import CPU (CPU(..))
import Expected (Expected(..))
import Interpreter
import MapUtils (mapAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op8XY1), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp8XY1 :: OpcodeCallback
execOp8XY1 interpreter (Op8XY1 args) = Expected interpreter { cpu = cpu' { v = mapAt (.|. vy) (int $ x args) (v cpu') } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vy = v' !! (int $ y args)
execOp8XY1 _ op = wrongOpcode "8XY1" op