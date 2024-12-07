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

import Destroy (SFMLResource(..), destroyAll)
import Either (fromLeft', fromRight')
import Expected (Expected(..))
import MapUtils (mapM2)
import Screen (Screen, Size, makeScreen, makeWindow, draw)

liftJoin2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f a b = join $ liftA2 f a b

exit :: Expected a -> IO ()
exit (Unexpected err) = die err
exit _ = exitSuccess

main :: IO ()
main = exit $ liftJoin2 (\window screen -> loop window screen >> destroyAll [SFMLResource window, SFMLResource screen]) makeWindow makeScreen

handleEvent :: RenderWindow -> Screen -> Maybe SFEvent -> Expected ()
handleEvent _ _ (Just SFEvtClosed) = return ()
handleEvent _ _ (Just (SFEvtKeyPressed KeyEscape _ _ _ _)) = return ()
handleEvent _ _ Nothing = return ()
handleEvent window screen _ = loop window screen

loop :: RenderWindow -> Screen -> Expected ()
loop window screen = liftIO (clearRenderWindow window black >> draw window screen Nothing >> display window >> waitEvent window) >>= handleEvent window screen
