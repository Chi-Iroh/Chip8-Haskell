{-# LANGUAGE InstanceSigs #-}
module Screen (Size, Screen, pixelSide, pixelSize, screenSize, makeWindow, makeScreen, draw, destroy) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)

import SFML.Graphics
import SFML.Window
import SFML.SFResource
import SFML.SFException

import Either
import Expected
import MapUtils (imapM2, mapM2_)
import SFVector (itoVec2f)

data Screen = Screen {
  px :: [[Bool]],
  rects :: [[RectangleShape]]
}

instance SFResource Screen where
    destroy :: Screen -> IO ()
    destroy = mapM2_ destroy . rects

instance SFDrawable Screen where
    draw :: SFRenderTarget t => t -> Screen -> Maybe RenderStates -> IO ()
    draw target screen renderStates = mapM2_ (\rect -> drawRectangle target rect renderStates) (rects screen)

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

makeScreen :: Expected Screen
makeScreen = fmap (makeScreen' pixels) rects
    where
        pixels = replicate (height_ screenSize) (replicate (width_ screenSize) False)
        rects = imapM2 (\y x color -> makePixel (pixelPos (x, y)) (if mod x 2 == mod y 2 then red else green)) pixels