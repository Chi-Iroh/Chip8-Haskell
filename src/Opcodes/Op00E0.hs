module Opcodes.Op00E0 (execOp00E0) where

import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op00E0), OpcodeArgs(..), OpcodeCallback)
import Screen (clearScreen)

execOp00E0 :: OpcodeCallback
execOp00E0 interpreter Op00E0 = Expected $ interpreter { screen = clearScreen (screen interpreter) }
execOp00E0 _ op = wrongOpcode "00E0" op