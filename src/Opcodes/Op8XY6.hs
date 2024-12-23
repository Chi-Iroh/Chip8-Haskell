module Opcodes.Op8XY6 (execOp8XY6) where

import Data.Bits ((.&.), (.>>.))

import CPU (CPU(..))
import Expected (Expected(..))
import Interpreter
import List (setAt)
import MapUtils (mapAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op8XY6), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp8XY6 :: OpcodeCallback
execOp8XY6 interpreter (Op8XY6 args) = Expected interpreter { cpu = cpu' { v = lsbSave } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vx = v' !! (int $ x args)
          lsb = vx .&. 0x01
          shifted = mapAt (.>>. 1) (int $ x args) v'
          lsbSave = setAt 0xF lsb shifted
execOp8XY6 _ op = wrongOpcode "8XY6" op