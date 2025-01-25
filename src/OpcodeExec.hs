module OpcodeExec (execFrameOpcodes) where

import CPU (incrementPc, decrementCounters)
import Expected
import Interpreter
import Opcode (readOpcode)

import Opcodes.Op00E0 (execOp00E0)
import Opcodes.Op00EE (execOp00EE)
import Opcodes.Op0NNN (execOp0NNN)
import Opcodes.Op1NNN (execOp1NNN)
import Opcodes.Op2NNN (execOp2NNN)
import Opcodes.Op3XNN (execOp3XNN)
import Opcodes.Op4XNN (execOp4XNN)
import Opcodes.Op5XY0 (execOp5XY0)
import Opcodes.Op6XNN (execOp6XNN)
import Opcodes.Op7XNN (execOp7XNN)
import Opcodes.Op8XY0 (execOp8XY0)
import Opcodes.Op8XY1 (execOp8XY1)
import Opcodes.Op8XY2 (execOp8XY2)
import Opcodes.Op8XY3 (execOp8XY3)
import Opcodes.Op8XY4 (execOp8XY4)
import Opcodes.Op8XY5 (execOp8XY5)
import Opcodes.Op8XY6 (execOp8XY6)
import Opcodes.Op8XY7 (execOp8XY7)
import Opcodes.Op8XYE (execOp8XYE)
import Opcodes.Op9XY0 (execOp9XY0)
import Opcodes.OpANNN (execOpANNN)
import Opcodes.OpBNNN (execOpBNNN)
import Opcodes.OpCXNN (execOpCXNN)
import Opcodes.OpDXYN (execOpDXYN)
import Opcodes.OpEX9E (execOpEX9E)
import Opcodes.OpEXA1 (execOpEXA1)
import Opcodes.OpFX07 (execOpFX07)
import Opcodes.OpFX0A (execOpFX0A)
import Opcodes.OpFX15 (execOpFX15)
import Opcodes.OpFX18 (execOpFX18)
import Opcodes.OpFX1E (execOpFX1E)
import Opcodes.OpFX29 (execOpFX29)
import Opcodes.OpFX33 (execOpFX33)
import Opcodes.OpFX55 (execOpFX55)
import Opcodes.OpFX65 (execOpFX65)

import OpcodeTypes (OpcodeCallback, Opcode(..))
import Sound (playSoundIfCounterOk)

opcodeFunc :: Opcode -> Expected OpcodeCallback
opcodeFunc (Op00E0) = Expected execOp00E0
opcodeFunc (Op00EE) = Expected execOp00EE
opcodeFunc (Op0NNN _) = Expected execOp0NNN
opcodeFunc (Op1NNN _) = Expected execOp1NNN
opcodeFunc (Op2NNN _) = Expected execOp2NNN
opcodeFunc (Op3XNN _) = Expected execOp3XNN
opcodeFunc (Op4XNN _) = Expected execOp4XNN
opcodeFunc (Op5XY0 _) = Expected execOp5XY0
opcodeFunc (Op6XNN _) = Expected execOp6XNN
opcodeFunc (Op7XNN _) = Expected execOp7XNN
opcodeFunc (Op8XY0 _) = Expected execOp8XY0
opcodeFunc (Op8XY1 _) = Expected execOp8XY1
opcodeFunc (Op8XY2 _) = Expected execOp8XY2
opcodeFunc (Op8XY3 _) = Expected execOp8XY3
opcodeFunc (Op8XY4 _) = Expected execOp8XY4
opcodeFunc (Op8XY5 _) = Expected execOp8XY5
opcodeFunc (Op8XY6 _) = Expected execOp8XY6
opcodeFunc (Op8XY7 _) = Expected execOp8XY7
opcodeFunc (Op8XYE _) = Expected execOp8XYE
opcodeFunc (Op9XY0 _) = Expected execOp9XY0
opcodeFunc (OpANNN _) = Expected execOpANNN
opcodeFunc (OpBNNN _) = Expected execOpBNNN
opcodeFunc (OpCXNN _) = Expected execOpCXNN
opcodeFunc (OpDXYN _) = Expected execOpDXYN
opcodeFunc (OpEX9E _) = Expected execOpEX9E
opcodeFunc (OpEXA1 _) = Expected execOpEXA1
opcodeFunc (OpFX07 _) = Expected execOpFX07
opcodeFunc (OpFX0A _) = Expected execOpFX0A
opcodeFunc (OpFX15 _) = Expected execOpFX15
opcodeFunc (OpFX18 _) = Expected execOpFX18
opcodeFunc (OpFX1E _) = Expected execOpFX1E
opcodeFunc (OpFX29 _) = Expected execOpFX29
opcodeFunc (OpFX33 _) = Expected execOpFX33
opcodeFunc (OpFX55 _) = Expected execOpFX55
opcodeFunc (OpFX65 _) = Expected execOpFX65
opcodeFunc op = Unexpected ("Opcode " ++ show op ++ " not implemented yet !")

incrementPc' :: Interpreter -> Expected Interpreter
incrementPc' interpreter = incrementPc (cpu interpreter) >>= (\cpu' -> Expected interpreter { cpu = cpu' })

execNextOpcode :: Interpreter -> Expected Interpreter
execNextOpcode interpreter = readOpcode (cpu interpreter) >>= (\op -> opcodeFunc op >>= (\callback -> callback interpreter op)) >>= incrementPc'

execMultipleOpcodes :: Int -> Interpreter -> Expected Interpreter
execMultipleOpcodes 0 interpreter = Expected interpreter
execMultipleOpcodes 1 interpreter = execNextOpcode interpreter
execMultipleOpcodes n interpreter = foldl (\interpreter' _ -> interpreter' >>= execNextOpcode) (Expected interpreter) [1..n]

opcodesPerFrame :: Int
opcodesPerFrame = 4

execFrameOpcodes :: Interpreter -> Expected Interpreter
execFrameOpcodes = execMultipleOpcodes opcodesPerFrame <$> (\interpreter' -> interpreter' { cpu =  decrementCounters $ playSoundIfCounterOk (cpu interpreter') (beep interpreter') })