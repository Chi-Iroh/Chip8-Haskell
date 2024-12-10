module Opcodes.Op8XY5 (execOp8XY5) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Foreign.Marshal.Utils (fromBool)
import Interpreter
import List (setAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op8XY5), OpcodeArgs(..), OpcodeCallback)
import Word (subUnderflow8, int)

execOp8XY5 :: OpcodeCallback
execOp8XY5 interpreter (Op8XY5 args) = Expected interpreter { cpu = cpu' { v = underflown } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vx = v' !! (int $ x args)
          vy = v' !! (int $ y args)
          (diff, underflow) = subUnderflow8 vx vy
          diffed = setAt (int $ x args) diff v'
          underflown = setAt 0xF (fromBool $ not underflow) diffed
execOp8XY5 _ op = wrongOpcode "8XY5" op