module Opcodes.Op8XY2 (execOp8XY2) where

import Data.Bits ((.&.))

import CPU (CPU(..))
import Expected (Expected(..))
import Interpreter
import MapUtils (mapAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op8XY2), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp8XY2 :: OpcodeCallback
execOp8XY2 interpreter (Op8XY2 args) = Expected interpreter { cpu = cpu' { v = mapAt (.&. vy) (int $ x args) (v cpu') } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vy = v' !! (int $ y args)
execOp8XY2 _ op = wrongOpcode "8XY2" op