module MapUtils (imap, imapM, imapM2, mapM2, mapM2_) where

imap :: (Int -> a -> b) -> [a] -> [b]
imap f arr = map (\(i, a) -> f i a) (zip [0..] arr)

imapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
imapM f arr = sequence $ imap f arr

imapM2 :: Monad m => (Int -> Int -> a -> m b) -> [[a]] -> m [[b]]
imapM2 f arr = imapM (\y -> imapM (f y)) arr

mapM2 :: Monad m => (a -> m b) -> [[a]] -> m [[b]]
mapM2 f arr = mapM (mapM f) arr

mapM2_ :: Monad m => (a -> m b) -> [[a]] -> m ()
mapM2_ f arr = mapM2 f arr >> pure ()