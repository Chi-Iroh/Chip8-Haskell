module Opcodes.Op8XY3 (execOp8XY3) where

import Data.Bits ((.^.))

import CPU (CPU(..))
import Expected (Expected(..))
import Interpreter
import MapUtils (mapAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op8XY3), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp8XY3 :: OpcodeCallback
execOp8XY3 interpreter (Op8XY3 args) = Expected interpreter { cpu = cpu' { v = mapAt (.^. vy) (int $ x args) (v cpu') } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vy = v' !! (int $ y args)
execOp8XY3 _ op = wrongOpcode "8XY3" op