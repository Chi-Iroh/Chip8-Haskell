module Opcodes.Op8XY4 (execOp8XY4) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Foreign.Marshal.Utils (fromBool)
import Interpreter
import List (setAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op8XY4), OpcodeArgs(..), OpcodeCallback)
import Word (addOverflow8, int)

execOp8XY4 :: OpcodeCallback
execOp8XY4 interpreter (Op8XY4 args) = Expected interpreter { cpu = cpu' { v = overflown } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vx = v' !! (int $ x args)
          vy = v' !! (int $ y args)
          (sum, overflow) = addOverflow8 vx vy
          summed = setAt (int $ x args) sum v'
          overflown = setAt 0xF (fromBool overflow) summed
execOp8XY4 _ op = wrongOpcode "8XY4" op