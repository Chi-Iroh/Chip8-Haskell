module Opcodes.OpDXYN (execOpDXYN) where

import Data.Word (Word8)

import CPU (CPU(..))
import Bits (toBits)
import Debug.Trace
import Expected (Expected(..))
import Foreign.Marshal.Utils (fromBool)
import Interpreter
import List (setAt, slice)
import MapUtils (imap)
import OpcodeTypes
import Opcodes.WrongOpcode (wrongOpcode)
import Screen (putPixels, Position)
import Word (int)

generateSpriteRowUpdates :: Position -> [Bool] -> [(Position, Bool)]
generateSpriteRowUpdates (x, y) spriteRow = imap (\i pos -> (pos, spriteRow !! i)) coords
    where coords = zip [x..(x + 7)] (repeat y)

generateSpriteUpdates :: Position -> Int -> [[Bool]] -> [(Position, Bool)]
generateSpriteUpdates (x, y) height sprite = concat $ imap (\i spriteRow -> generateSpriteRowUpdates (x, y + i) spriteRow) sprite

execOpDXYN :: OpcodeCallback
execOpDXYN interpreter (OpDXYN args) = Expected (interpreter { screen = updatedScreen, cpu = cpu' { v = v'' } })
    where cpu' = cpu interpreter
          screen' = screen interpreter
          sprite = map toBits (slice (int $ i cpu') (int (i cpu') + int (n args)) (memory cpu'))
          v' = v cpu'
          x' = int (v' !! (int $ x args))
          y' = int (v' !! (int $ y args))
          spriteChanges = generateSpriteUpdates (x', y') (int $ n args) sprite
          updatedScreen = putPixels spriteChanges screen'
          isScreenChanged = updatedScreen /= screen'
          v'' = setAt 0xF (fromBool isScreenChanged) v'
execOpDXYN _ op = wrongOpcode "DXYN" op