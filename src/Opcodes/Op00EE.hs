module Opcodes.Op00EE (execOp00EE) where

import Data.Maybe (fromJust)

import CPU
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op00EE), OpcodeArgs(..), OpcodeCallback)

execOp00EE :: OpcodeCallback
execOp00EE interpreter Op00EE
    | null jumps' = Unexpected "Cannot return from a subroutine because we weren't inside one !"
    | otherwise = checkPc (lastPc) >>= (\pc' -> Expected interpreter { cpu = cpu' { pc = pc', jumps = remainingJumps } })
    where cpu' = cpu interpreter
          jumps' = jumps cpu'
          lastPc = head jumps'
          remainingJumps = tail remainingJumps
execOp00EE _ op = wrongOpcode "00EE" op