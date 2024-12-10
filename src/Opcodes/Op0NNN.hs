module Opcodes.Op0NNN (execOp0NNN) where

import Expected (Expected(..))
import Interpreter (Interpreter(..))
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(Op0NNN), OpcodeArgs(..), OpcodeCallback)

execOp0NNN :: OpcodeCallback
execOp0NNN interpreter (Op0NNN _) = Expected interpreter
execOp0NNN _ op = wrongOpcode "0NNN" op