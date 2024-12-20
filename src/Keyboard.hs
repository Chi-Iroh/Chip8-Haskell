module Keyboard (isKeyPressed, waitForChip8Key, nthChip8Key, chip8KeyValue) where

import Data.List (find, (!?), elemIndex)
import Data.Maybe (fromJust, isNothing)
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

import SFML.Graphics (RenderWindow, waitEvent)
import SFML.Window.Keyboard (KeyCode(..))
import qualified SFML.Window.Keyboard (isKeyPressed)
import SFML.Window.Event (SFEvent(..))

import Expected (Expected (..), expected, fromMaybe)
import Word (int, u8)

-- Chip8 keyboard
-- 1 2 3 C
-- 4 5 6 D
-- 7 8 9 E
-- A 0 B F
data Chip8Key = Chip8Key_1 |
                Chip8Key_2 |
                Chip8Key_3 |
                Chip8Key_C |
                Chip8Key_4 |
                Chip8Key_5 |
                Chip8Key_6 |
                Chip8Key_D |
                Chip8Key_7 |
                Chip8Key_8 |
                Chip8Key_9 |
                Chip8Key_E |
                Chip8Key_A |
                Chip8Key_0 |
                Chip8Key_B |
                Chip8Key_F deriving (Eq, Show)

-- Bound to
-- 1& 2é 3" 4'
-- a  z  e  r
-- q  s  d  f
-- w  x  c  v
chip8Keys :: [(KeyCode, Chip8Key)]
chip8Keys = [ (KeyNum1, Chip8Key_1)
            , (KeyNum2, Chip8Key_2)
            , (KeyNum3, Chip8Key_3)
            , (KeyNum4, Chip8Key_C)

            , (KeyA, Chip8Key_4)
            , (KeyZ, Chip8Key_5)
            , (KeyE, Chip8Key_6)
            , (KeyR, Chip8Key_D)

            , (KeyQ, Chip8Key_7)
            , (KeyS, Chip8Key_8)
            , (KeyD, Chip8Key_9)
            , (KeyF, Chip8Key_E)

            , (KeyW, Chip8Key_A)
            , (KeyX, Chip8Key_0)
            , (KeyC, Chip8Key_B)
            , (KeyV, Chip8Key_F) ]

toChip8Key :: KeyCode -> Maybe Chip8Key
toChip8Key key = fmap snd (find ((== key) . fst) chip8Keys)

chip8KeysList :: [Chip8Key]
chip8KeysList = [ Chip8Key_0
                , Chip8Key_1
                , Chip8Key_2
                , Chip8Key_3
                , Chip8Key_4
                , Chip8Key_5
                , Chip8Key_6
                , Chip8Key_7
                , Chip8Key_8
                , Chip8Key_9
                , Chip8Key_A
                , Chip8Key_B
                , Chip8Key_C
                , Chip8Key_D
                , Chip8Key_E
                , Chip8Key_F ]

nthChip8Key :: Word8 -> Expected Chip8Key
nthChip8Key i = fromMaybe ("Chip8 key range is 0x0-0xF, but got " ++ show i) (chip8KeysList !? (int i))

chip8KeyValue :: Chip8Key -> Word8
chip8KeyValue key = u8 $ fromJust $ elemIndex key chip8KeysList

toKeyCode :: Chip8Key -> KeyCode
toKeyCode chip8Key = fst $ fromJust $ find ((== chip8Key) . snd) chip8Keys

eventToKeyCode :: SFEvent -> Maybe KeyCode
eventToKeyCode (SFEvtKeyPressed keycode _ _ _ _) = Just keycode
eventToKeyCode _ = Nothing

isKeyPressed :: Chip8Key -> Bool
isKeyPressed chip8Key = unsafePerformIO $! SFML.Window.Keyboard.isKeyPressed (toKeyCode chip8Key)

waitForChip8Key :: RenderWindow -> Chip8Key
waitForChip8Key window
    | isNothing event = waitForChip8Key window
    | isNothing keyEvent = waitForChip8Key window -- event isn't a keyboard event
    | isNothing chip8Key = waitForChip8Key window -- pressed key isn't a CHIP-8 key
    | otherwise = fromJust chip8Key
    where event = unsafePerformIO $! waitEvent window
          keyEvent = eventToKeyCode (fromJust event)
          chip8Key = toChip8Key (fromJust keyEvent)