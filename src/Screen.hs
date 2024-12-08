{-# LANGUAGE InstanceSigs #-}
module Screen (Size, Screen, pixelSide, pixelSize, screenSize, makeWindow, makeScreen, draw, destroy, putPixel, putPixels, swapPixel, swapPixels, swapAllPixels, generatePixels) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Data.List (find)
import Data.Maybe (maybe)
import Debug.Trace

import SFML.Graphics
import SFML.Window
import SFML.SFResource
import SFML.SFException

import Either
import Expected
import MapUtils (map2, imapM2, mapM2_, imap, imap2)
import SFVector (itoVec2f)

data Screen = Screen {
  px :: [[Bool]],
  rects :: [[RectangleShape]]
}

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

makeWindow :: Expected RenderWindow
makeWindow = liftIO $ createRenderWindow (VideoMode (width * pixelSide) (height * pixelSide) 32) "Chip8" [SFDefaultStyle] Nothing
    where (width, height) = screenSize

fromSFException :: SFException -> String
fromSFException (SFException err) = err

makePixel :: Position -> Color -> Expected RectangleShape
makePixel pos color = liftIO createRectangleShape >>= (\rect -> if isLeft rect then Unexpected (fromSFException $ fromLeft' rect) else setupRect (fromRight' rect) pos pixelSize color)

setupRect :: RectangleShape -> Position -> Size -> Color -> Expected RectangleShape
setupRect rect pos size color = liftIO (setPosition rect pos' >> setSize rect size' >> setFillColor rect color) >> Expected rect
    where pos' = itoVec2f pos
          size' = itoVec2f size

makeScreen' :: [[Bool]] -> [[RectangleShape]] -> Screen
makeScreen' px rects = Screen {
    px = px,
    rects = rects
}

emptyPixels :: [[Bool]]
emptyPixels = replicate (height_ screenSize) (replicate (width_ screenSize) False)

makeScreen :: Expected Screen
makeScreen = fmap (makeScreen' pixels) rects
    where
        pixels = emptyPixels
        rects = imapM2 (\x y color -> makePixel (pixelPos (x, y)) black) pixels

findWithDefault :: (a -> Bool) -> b -> (a -> b) -> [a] -> b
findWithDefault f default' convert arr = maybe default' convert (find f arr)

putPixels :: [(Position, Bool)] -> Screen -> Screen
putPixels changed screen = Screen {
    px = imap2 (\x y color' -> findWithDefault ((== (x, y)) . fst) color' snd changed) (px screen),
    rects = rects screen
}

putPixel :: Position -> Bool -> Screen -> Screen
putPixel pos color = putPixels [(pos, color)]

generatePixels :: (Int -> Int -> Bool -> Bool) -> Screen -> Screen
generatePixels f screen = Screen {
    px = imap2 f (px screen),
    rects = rects screen
}

swapPixels :: [Position] -> Screen -> Screen
swapPixels swapped screen = Screen {
    px = imap2 (\x y color -> if elem (x, y) swapped then not color else color) (px screen),
    rects = rects screen
}

swapPixel :: Position -> Screen -> Screen
swapPixel pos = swapPixels [pos]

swapAllPixels :: Screen -> Screen
swapAllPixels screen = Screen {
    px = map2 not (px screen),
    rects = rects screen
}

clearScreen :: Screen -> Screen
clearScreen screen = Screen {
    px = emptyPixels,
    rects = rects screen
}