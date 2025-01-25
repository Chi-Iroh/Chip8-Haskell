module Opcodes.OpFX0A (execOpFX0A) where

import Debug.Trace

import CPU (CPU(..), decrementCounters)
import Expected (Expected(..))
import Keyboard (waitForChip8Key, chip8KeyValue)
import Interpreter (Interpreter(..))
import List (setAt)
import Opcodes.WrongOpcode (wrongOpcode)
import OpcodeTypes (Opcode(OpFX0A), OpcodeArgs(..), OpcodeCallback)
import Word (int)

execOpFX0A :: OpcodeCallback
execOpFX0A interpreter (OpFX0A args) = Expected interpreter { cpu = cpu' { v = vWithKey } }
    where cpu' = cpu interpreter
          v' = v cpu'
          window' = window interpreter
          key = chip8KeyValue (waitForChip8Key window')
          vWithKey = setAt (int $ x args) key v'
execOpFX0A _ op = wrongOpcode "FX0A" op