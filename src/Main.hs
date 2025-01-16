module Main (main) where

import Data.Functor ((<&>))
import System.Exit (die, exitSuccess)

import SFML.Window (SFEvent(SFEvtClosed, SFEvtKeyPressed), KeyCode(..))
import SFML.Graphics(RenderWindow, clearRenderWindow, green)

import CPU (CPU(soundCounter))
import Expected (Expected(..), expected)
import Interpreter
import OpcodeExec (execFrameOpcodes)
import Screen (Screen, Size, updateScreen)

main' :: IO (Expected Interpreter) -> IO ()
main' interpreter = interpreter >>= loop >>= destroyInterpreter

main :: IO ()
main = main' makeInterpreter

handleEvent :: Expected Interpreter -> Maybe SFEvent -> IO (Expected Interpreter)
handleEvent err@(Unexpected _) _ = pure err
handleEvent interpreter@(Expected _) Nothing = loop interpreter
handleEvent interpreter@(Expected _) (Just SFEvtClosed) = pure interpreter
handleEvent interpreter@(Expected _) (Just (SFEvtKeyPressed KeyEscape _ _ _ _)) = pure interpreter
handleEvent interpreter@(Expected _) _ = loop interpreter

loop :: Expected Interpreter -> IO (Expected Interpreter)
loop interpreter = pure (interpreter >>= execFrameOpcodes) >>= (\interpreter' -> updateScreen' interpreter' >>= handleEvent interpreter')
    where updateScreen' interpreter' = case interpreter' of
            err@(Unexpected _) -> pure Nothing
            (Expected interpreter'') -> updateScreen (window interpreter'') (screen interpreter'')
