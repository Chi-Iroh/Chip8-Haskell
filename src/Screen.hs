{-# LANGUAGE InstanceSigs #-}
module Screen (
    Position,
    Size,
    Screen,
    pixelSide,
    pixelSize,
    screenSize,
    makeWindow,
    makeScreen,
    draw,
    destroy,
    putPixel,
    putPixels,
    swapPixel,
    swapPixels,
    swapAllPixels,
    generatePixels,
    clearScreen,
    sleep,
    updateScreen,
    zip2,
    px,
    makeScreen',
    emptyPixels
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (maybe)
import Debug.Trace

import SFML.Graphics
import SFML.Window
import SFML.SFResource
import SFML.SFException
import SFML.System.Sleep (sfSleep)
import SFML.System.Time (Time(..), microseconds)

import Either
import Expected (Expected(..))
import MapUtils (map2, imapM2, mapM2_, imap, imap2)
import SFVector (itoVec2f)

data Screen = Screen {
  px :: [[Bool]],
  rects :: [[RectangleShape]]
}

instance Eq Screen where
    (==) :: Screen -> Screen -> Bool
    (==) a b = (px a) == (px b)

    (/=) :: Screen -> Screen -> Bool
    (/=) a b = (px a) /= (px b)

instance Show Screen where
    show :: Screen -> String
    show = show . concat . px

instance SFResource Screen where
    destroy :: Screen -> IO ()
    destroy = mapM2_ destroy . rects

zip2 :: [[a]] -> [[b]] -> [[(a, b)]]
zip2 = zipWith zip

boolColor :: Bool -> Color
boolColor True = white
boolColor False = black

instance SFDrawable Screen where
    draw :: SFRenderTarget t => t -> Screen -> Maybe RenderStates -> IO ()
    draw target screen renderStates = mapM2_ (\(isWhite, rect) -> setFillColor rect (boolColor isWhite) >> drawRectangle target rect renderStates) (zip2 px' rects')
        where rects' = rects screen
              px' = px screen

pixelSide :: Int
pixelSide = 8

type Size = (Int, Int)
type Position = Size

pixelSize :: Size
pixelSize = (pixelSide, pixelSide)

pixelPos :: (Int, Int) -> Position
pixelPos (x, y) = (x * px, y * py)
    where (px, py) = pixelSize

screenSize :: Size
screenSize = (64, 32)

width_ :: Size -> Int
width_ = fst

height_ :: Size -> Int
height_ = snd

int64 :: Int -> Int64
int64 = toEnum

fps :: Int
fps = 60

sleepTime :: Time
sleepTime = microseconds (div 1000000 (int64 fps))

sleep :: IO ()
sleep = sfSleep sleepTime

makeWindow :: IO (Expected RenderWindow)
makeWindow = window >>= (\window' -> setFramerateLimit window' fps >> return (Expected window'))
    where (width, height) = screenSize
          window = createRenderWindow (VideoMode (width * pixelSide) (height * pixelSide) 32) "Chip8" [SFDefaultStyle] Nothing

fromSFException :: SFException -> String
fromSFException (SFException err) = err

makePixel :: Position -> Color -> IO (Expected RectangleShape)
makePixel pos color = createRectangleShape >>= getRect
    where getRect rect = case rect of
            Left err -> pure (Unexpected (fromSFException err))
            Right rect' -> setupRect rect' pos pixelSize color

setupRect :: RectangleShape -> Position -> Size -> Color -> IO (Expected RectangleShape)
setupRect rect pos size color = setPosition rect pos' >> setSize rect size' >> setFillColor rect color >> pure (Expected rect)
    where pos' = itoVec2f pos
          size' = itoVec2f size

makeScreen' :: [[Bool]] -> [[RectangleShape]] -> Screen
makeScreen' px rects = Screen {
    px = px,
    rects = rects
}

emptyPixels :: [[Bool]]
emptyPixels = replicate (height_ screenSize) (replicate (width_ screenSize) False)

sequence2 :: Monad m => [[m a]] -> m [[a]]
sequence2 = sequence . (map sequence)

makeScreen :: IO (Expected Screen)
makeScreen = rects <&> (fmap (makeScreen' pixels))
    where
        pixels = emptyPixels
        rects = imapM2 (\x y color -> makePixel (pixelPos (x, y)) black) pixels <&> sequence2

findWithDefault :: (a -> Bool) -> b -> (a -> b) -> [a] -> b
findWithDefault f default' convert arr = maybe default' convert (find f arr)

-- simply ignores out of range positions
putPixels :: [(Position, Bool)] -> Screen -> Screen
putPixels changed screen = Screen {
    px = imap2 (\x y color' -> findWithDefault ((== (x, y)) . fst) color' snd changed) (px screen),
    rects = rects screen
}

-- simply ignores out of range position
putPixel :: Position -> Bool -> Screen -> Screen
putPixel pos color = putPixels [(pos, color)]

generatePixels :: (Int -> Int -> Bool -> Bool) -> Screen -> Screen
generatePixels f screen = Screen {
    px = imap2 f (px screen),
    rects = rects screen
}

-- simply ignores out of range positions
swapPixels :: [Position] -> Screen -> Screen
swapPixels swapped screen = Screen {
    px = imap2 (\x y color -> if elem (x, y) swapped then not color else color) (px screen),
    rects = rects screen
}

-- simply ignores out of range position
swapPixel :: Position -> Screen -> Screen
swapPixel pos = swapPixels [pos]

swapAllPixels :: Screen -> Screen
swapAllPixels screen = Screen {
    px = map2 not (px screen),
    rects = rects screen
}

clearScreen :: Screen -> Screen
clearScreen screen = screen {
    px = emptyPixels
}

updateScreen :: RenderWindow -> Screen -> IO (Maybe SFEvent)
updateScreen window screen = sleep >> clearRenderWindow window black >> draw window screen Nothing >> display window >> pollEvent window