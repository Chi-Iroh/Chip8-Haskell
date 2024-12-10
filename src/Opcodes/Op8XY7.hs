module Opcodes.Op8XY7 (execOp8XY7) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Foreign.Marshal.Utils (fromBool)
import Interpreter
import List (setAt)
import MapUtils (mapAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op8XY7), OpcodeArgs(..), OpcodeCallback)
import Word (subUnderflow8, int)

execOp8XY7 :: OpcodeCallback
execOp8XY7 interpreter (Op8XY7 args) = Expected interpreter { cpu = cpu' { v = underflown } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vx = v' !! (int $ x args)
          vy = v' !! (int $ y args)
          (diff, underflow) = subUnderflow8 vy vx
          diffed = setAt (int $ x args) diff v'
          underflown = setAt 0xF (fromBool $ not underflow) diffed
execOp8XY7 _ op = wrongOpcode "8XY7" op