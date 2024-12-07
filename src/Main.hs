module Main where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
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

window :: Expected RenderWindow
window = liftIO $ createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] Nothing

fromSFException :: SFException -> String
fromSFException (SFException err) = err

setupRect :: RectangleShape -> Expected RectangleShape
setupRect rect = liftIO (setFillColor rect white >> setPosition rect (Vec2f 100 50) >> setSize rect (Vec2f 50 50)) >> Expected rect

rect :: Expected RectangleShape
rect = liftIO createRectangleShape >>= (\rect' -> if isLeft rect' then Unexpected (fromSFException $ fromLeft' rect') else setupRect (fromRight' rect'))

liftJoin2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f a b = join $ liftA2 f a b

destroy' :: SFML.SFResource.SFResource a => a -> Expected ()
destroy' a = liftIO (destroy a)

exit :: Expected a -> IO ()
exit (Unexpected err) = die err
exit _ = exitSuccess

main :: IO ()
main = exit $ liftJoin2 (\window' rect' -> loop window' rect' >> destroy' window' >> destroy' rect') window rect

handleEvent :: RenderWindow -> RectangleShape -> Maybe SFEvent -> Expected ()
handleEvent _ _ (Just SFEvtClosed) = return ()
handleEvent _ _ Nothing = return ()
handleEvent window' rect' _ = loop window' rect'

loop :: RenderWindow -> RectangleShape -> Expected ()
loop window' rect' = liftIO (clearRenderWindow window' blue >> drawRectangle window' rect' Nothing >> display window' >> waitEvent window') >>= handleEvent window' rect'
