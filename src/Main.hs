module Main (main) where

import Debug.Trace (traceShowId)
import System.Exit (die, exitSuccess)

import SFML.Window (SFEvent(SFEvtClosed, SFEvtKeyPressed), KeyCode(..))
import SFML.Graphics(RenderWindow)

import CPU (CPU(soundCounter))
import Expected (Expected(..), liftIO)
import Interpreter
import OpcodeExec (execFrameOpcodes)
import Screen (Screen, Size, updateScreen)

exit :: Show a => Expected a -> IO ()
exit (Unexpected err) = die ("Error: " ++ err)
exit (Expected a) = print a >> exitSuccess

main' :: Expected Interpreter -> Expected ()
main' interpreter = interpreter >>= loop >>= destroyInterpreter

main :: IO ()
main = fmap (main' . traceShowId) makeInterpreter >>= exit

handleEvent :: Interpreter -> Maybe SFEvent -> Expected Interpreter
handleEvent interpreter Nothing = loop interpreter
handleEvent interpreter (Just SFEvtClosed) = Expected interpreter
handleEvent interpreter (Just (SFEvtKeyPressed KeyEscape _ _ _ _)) = Expected interpreter
handleEvent interpreter _ = loop interpreter

loop :: Interpreter -> Expected Interpreter
loop interpreter = execFrameOpcodes interpreter >>= (\interpreter' -> updateScreen (window interpreter') (screen interpreter') >>= handleEvent interpreter')
