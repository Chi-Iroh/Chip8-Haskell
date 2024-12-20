module Opcodes.OpFX29 (execOpFX29) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Font (digitSize)
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpFX29), OpcodeArgs(..), OpcodeCallback)
import Word (int, u16)

replaceAt :: Int -> [a] -> [a] -> [a]
replaceAt 0 new arr = new ++ (drop (length new) arr)
replaceAt n new arr = take n arr ++ replaceAt 0 new (drop n arr)

execOpFX29 :: OpcodeCallback
execOpFX29 interpreter (OpFX29 args) = Expected interpreter { cpu = cpu' { i = u16 $ digitSize * (int vx) } }
    where cpu' = cpu interpreter
          v' = v cpu'
          vx = v' !! (int $ x args)
execOpFX29 _ op = wrongOpcode "FX29" op