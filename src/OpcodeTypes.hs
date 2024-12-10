{-# LANGUAGE InstanceSigs #-}
module OpcodeTypes where

import Expected (Expected(..))
import Hex
import Interpreter (Interpreter(..))
import Word

type X = Word8
type Y = Word8
type N = Word8
type NN = Word8
type NNN = Word16

data OpcodeArgs = OpcodeArgs {
    x :: X,
    y :: Y,
    n :: N,
    nn :: NN,
    nnn :: NNN
}

instance Show OpcodeArgs where
    show :: OpcodeArgs -> String
    show args = "OpcodeArgs { x = " ++ showHex8 (x args) ++ ", y = " ++ showHex8 (y args) ++ ", n = " ++ showHex8 (n args) ++ ", nn = " ++ showHex8 (nn args) ++ ", nnn = " ++ showHex16 (nnn args) ++ " }"

data Opcode =   Op0NNN OpcodeArgs   |
                Op00E0              |
                Op00EE              |
                Op1NNN OpcodeArgs   |
                Op2NNN OpcodeArgs   |
                Op3XNN OpcodeArgs   |
                Op4XNN OpcodeArgs   |
                Op5XY0 OpcodeArgs   |
                Op6XNN OpcodeArgs   |
                Op7XNN OpcodeArgs   |
                Op8XY0 OpcodeArgs   |
                Op8XY1 OpcodeArgs   |
                Op8XY2 OpcodeArgs   |
                Op8XY3 OpcodeArgs   |
                Op8XY4 OpcodeArgs   |
                Op8XY5 OpcodeArgs   |
                Op8XY6 OpcodeArgs   |
                Op8XY7 OpcodeArgs   |
                Op8XYE OpcodeArgs   |
                Op9XY0 OpcodeArgs   |
                OpANNN OpcodeArgs   |
                OpBNNN OpcodeArgs   |
                OpCXNN OpcodeArgs   |
                OpDXYN OpcodeArgs   |
                OpEX9E OpcodeArgs   |
                OpEXA1 OpcodeArgs   |
                OpFX07 OpcodeArgs   |
                OpFX0A OpcodeArgs   |
                OpFX15 OpcodeArgs   |
                OpFX18 OpcodeArgs   |
                OpFX1E OpcodeArgs   |
                OpFX29 OpcodeArgs   |
                OpFX33 OpcodeArgs   |
                OpFX55 OpcodeArgs   |
                OpFX65 OpcodeArgs deriving Show

type OpcodeCallback = Interpreter -> Opcode -> Expected Interpreter