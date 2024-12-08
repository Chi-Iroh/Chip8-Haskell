module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import System.Exit (die, exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

import SFML.Window (display, SFWindow(waitEvent), SFEvent(SFEvtClosed, SFEvtKeyPressed), KeyCode(..))
import SFML.Graphics
    ( black,
      clearRenderWindow,
      SFDrawable(draw),
      RenderWindow,
      display,
      waitEvent )

import Args (readROM)
import CPU (CPU(..), Word8)
import Destroy (SFMLResource(..), destroyAll)
import Either (fromLeft', fromRight')
import Expected (Expected(..))
import MapUtils (mapM2)
import Screen (Screen, Size, makeScreen, makeWindow, draw, swapAllPixels, generatePixels, sleep)

liftJoin2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f a b = join $ liftA2 f a b

exit :: Show a => Expected a -> IO ()
exit (Unexpected err) = die err
exit (Expected a) = print a >> exitSuccess

main :: IO ()
-- main = readROM >>= exit
main = exit $ liftJoin2 (\window screen -> loop window (generatePixels (\x y _ -> (rem x (y + 1)) == 0) screen) >> destroyAll [SFMLResource window, SFMLResource screen]) makeWindow makeScreen

handleEvent :: RenderWindow -> Screen -> Maybe SFEvent -> Expected Screen
handleEvent _ screen Nothing = return screen
handleEvent _ screen (Just SFEvtClosed) = return screen
handleEvent _ screen (Just (SFEvtKeyPressed KeyEscape _ _ _ _)) = return screen
handleEvent window screen (Just (SFEvtKeyPressed KeySpace _ _ _ _)) = loop window (swapAllPixels screen)
handleEvent window screen _ = loop window screen

loop :: RenderWindow -> Screen -> Expected Screen
loop window screen = liftIO (sleep >> clearRenderWindow window black >> draw window screen Nothing >> display window >> waitEvent window) >>= handleEvent window screen
