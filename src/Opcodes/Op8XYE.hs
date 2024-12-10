module Opcodes.Op8XYE (execOp8XYE) where

import Data.Bits ((.<<.), (.&.), (.>>.))

import CPU (CPU(..))
import Expected (Expected(..))
import Interpreter
import List (setAt)
import MapUtils (mapAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op8XYE), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOp8XYE :: OpcodeCallback
execOp8XYE interpreter (Op8XYE args) = Expected interpreter { cpu = cpu' { v = msbSave } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vx = v' !! (int $ x args)
          msb = (vx .&. 0x80) .>>. 7
          shifted = mapAt (.<<. 1) (int $ x args) v'
          msbSave = setAt 0xF msb shifted
execOp8XYE _ op = wrongOpcode "8XYE" op