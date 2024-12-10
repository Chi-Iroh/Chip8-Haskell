module List where

import Data.List ((!?))
import Data.Maybe (maybeToList)

replaceAt :: Int -> [a] -> [a] -> [a]
replaceAt 0 new arr = new ++ (drop (length new) arr)
replaceAt n new arr = take n arr ++ replaceAt 0 new (drop n arr)

setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 a (_ : xs) = a : xs
setAt n a (x : xs) = x : setAt (n - 1) a xs

slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice start end arr
    | start == end = maybeToList (arr !? start)
    | start > end = reverse (slice end start arr)
    | otherwise = take (end - start) (drop start arr)