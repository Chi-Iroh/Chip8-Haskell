module SFVector (toVec2f, itoVec2f) where

import GHC.Float (int2Float)
import SFML.System.Vector2 (Vec2f(..))

toVec2f :: (Float, Float) -> Vec2f
toVec2f (x, y) = Vec2f x y

itoVec2f :: (Int, Int) -> Vec2f
itoVec2f (x, y) = toVec2f (int2Float x, int2Float y)