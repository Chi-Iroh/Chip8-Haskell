module Opcodes.OpANNN (execOpANNN) where

import CPU (checkPc, CPU(..))
import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpANNN), OpcodeArgs(..), OpcodeCallback)

snoc :: a -> [a] -> [a]
snoc a arr = arr ++ [a]

execOpANNN :: OpcodeCallback
execOpANNN interpreter (OpANNN args) = Expected interpreter { cpu = cpu' { i = nnn args } }
    where cpu' = cpu interpreter
execOpANNN _ op = wrongOpcode "ANNN" op