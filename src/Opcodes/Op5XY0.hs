module Opcodes.Op5XY0 (execOp5XY0) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Foreign.Marshal.Utils (fromBool)
import Interpreter
import List (setAt)
import MapUtils (mapAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op5XY0), OpcodeArgs(..), OpcodeCallback)
import Word (addOverflow8)
import Word (int)

execOp5XY0 :: OpcodeCallback
execOp5XY0 interpreter (Op5XY0 args) = Expected interpreter { cpu = cpu' { pc = if vx == vy then pc' + 2 else pc' } }
    where cpu' = cpu interpreter
          pc' = pc cpu'
          v' = v cpu'
          vx = v' !! (int $ x args)
          vy = v' !! (int $ y args)
execOp5XY0 _ op = wrongOpcode "5XY0" op