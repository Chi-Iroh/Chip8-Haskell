module Opcodes.OpCXNN (execOpCXNN) where

import Data.Bits ((.&.))
import Data.Word (Word8)
import System.Random (randomR, StdGen)

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import List (setAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpCXNN), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpCXNN :: OpcodeCallback
execOpCXNN interpreter (OpCXNN args) = Expected interpreter {
        cpu = cpu' { v = setAt (int $ x args) ((nn args) .&. rnd) (v cpu') },
        seed = seed'
    }
    where cpu' = cpu interpreter
          (rnd, seed') = randomR (0x00, 0xFF) (seed interpreter) :: (Word8, StdGen)
execOpCXNN _ op = wrongOpcode "CXNN" op