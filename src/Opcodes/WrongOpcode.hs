module Opcodes.WrongOpcode (wrongOpcode) where

import Expected (Expected(Unexpected))
import Interpreter (Interpreter)
import OpcodeTypes (Opcode)

wrongOpcode :: String -> Opcode -> Expected Interpreter
wrongOpcode expected got = Unexpected $ "Opcode " ++ expected ++ " was called with a bad opcode " ++ show got ++ " !"