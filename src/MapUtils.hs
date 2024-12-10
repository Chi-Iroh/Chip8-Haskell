module MapUtils where

import Control.Monad (void)
import Data.List ((!?))

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)

imap :: (Int -> a -> b) -> [a] -> [b]
imap f arr = map (\(i, a) -> f i a) (zip [0..] arr)

imap2 :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
imap2 f = imap (\y -> imap (\x -> f x y))

imapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
imapM f arr = sequence $ imap f arr

imapM2 :: Monad m => (Int -> Int -> a -> m b) -> [[a]] -> m [[b]]
imapM2 f = imapM (\y -> imapM (\x -> f x y))

mapM2 :: Monad m => (a -> m b) -> [[a]] -> m [[b]]
mapM2 f = mapM (mapM f)

mapM2_ :: Monad m => (a -> m b) -> [[a]] -> m ()
mapM2_ f arr = void $ mapM2 f arr

mapAt :: (a -> a) -> Int -> [a] -> [a]
mapAt f i = imap (\i' a -> if i == i' then f a else a)