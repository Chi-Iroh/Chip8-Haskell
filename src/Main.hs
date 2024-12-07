{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import System.Exit (die, exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

import SFML.Graphics
    ( blue,
      white,
      createRectangleShape,
      setSize,
      clearRenderWindow,
      createRenderWindow,
      SFRenderTarget(drawRectangle),
      SFShape(setFillColor),
      SFTransformable(setPosition),
      RectangleShape,
      RenderWindow,
      display,
      destroy,
      waitEvent )
import SFML.Window
    ( display,
      destroy,
      SFWindow(waitEvent),
      Vec2f(Vec2f),
      SFEvent(SFEvtClosed),
      VideoMode(VideoMode),
      WindowStyle(SFDefaultStyle) )
import SFML.SFException (SFException(..))
import SFML.SFResource (SFResource)

import Debug (debug2)
import Either (fromLeft', fromRight')
import Expected (Expected(..))
import MapUtils (mapM2)
import Screen (Screen, Size, makeScreen, makeWindow, draw)

liftJoin2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f a b = join $ liftA2 f a b

data SFMLResource = forall a. SFML.SFResource.SFResource a => SFMLResource a

destroy' :: SFMLResource -> Expected ()
destroy' (SFMLResource a) = liftIO (destroy a)

destroyAll :: [SFMLResource] -> Expected ()
destroyAll = mapM_ destroy'

exit :: Expected a -> IO ()
exit (Unexpected err) = die err
exit _ = exitSuccess

main :: IO ()
main = exit $ liftJoin2 (\window screen -> loop window screen >> destroyAll [SFMLResource window, SFMLResource screen]) makeWindow makeScreen

handleEvent :: RenderWindow -> Screen -> Maybe SFEvent -> Expected ()
handleEvent _ _ (Just SFEvtClosed) = return ()
handleEvent _ _ Nothing = return ()
handleEvent window screen _ = loop window screen

loop :: RenderWindow -> Screen -> Expected ()
loop window screen = liftIO (clearRenderWindow window blue >> draw window screen Nothing >> display window >> waitEvent window) >>= handleEvent window screen
