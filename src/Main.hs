module Main where
import Control.Monad
import Data.Either
import Debug.Trace
import SFML.Graphics
import SFML.Window
import SFML.SFException

window :: IO RenderWindow
window = createRenderWindow (VideoMode 640 480 32) "SFML Haskell Demo" [SFDefaultStyle] Nothing

fromSFException :: SFException -> String
fromSFException (SFException err) = traceShowId err

fromLeft' :: Either a b -> a
fromLeft' (Left a) = a
fromLeft' _ = error "Nothing at Right !"

fromRight' :: Either a b -> b
fromRight' (Right b) = b
fromRight' _ = error "Nothing at Left !"

setupRect :: RectangleShape -> IO RectangleShape
setupRect rect = setFillColor rect white >> setPosition rect (Vec2f 100 50) >> setSize rect (Vec2f 50 50) >> return rect

rect :: IO RectangleShape
rect = createRectangleShape >>= (\rect' -> if isLeft rect' then fail (fromSFException $ fromLeft' rect') else setupRect $ fromRight' rect')

maybeLeft :: Either a b -> Maybe a
maybeLeft (Left a) = Just a
maybeLeft _ = Nothing

maybeRight :: Either a b -> Maybe b
maybeRight (Right b) = Just b
maybeRight _ = Nothing

liftJoin2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f a b = join $ liftA2 f a b

main :: IO ()
main = liftJoin2 (\window' rect' -> loop window' rect' >> destroy window' >> destroy rect') window rect

handleEvent :: RenderWindow -> RectangleShape -> Maybe SFEvent -> IO ()
handleEvent _ _ (Just SFEvtClosed) = return ()
handleEvent _ _ Nothing = return ()
handleEvent window' rect' _ = loop window' rect'

loop :: RenderWindow -> RectangleShape -> IO ()
loop window' rect' = clearRenderWindow window' blue >> drawRectangle window' rect' Nothing >> display window' >> waitEvent window' >>= (handleEvent window' rect')
