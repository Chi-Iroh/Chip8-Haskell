{-# LANGUAGE InstanceSigs #-}
module Sound (Sound(..), makeSound, playSoundIfCounterOk) where

import Control.Monad (join)
import System.IO.Unsafe (unsafePerformIO)

import qualified SFML.Audio (SoundBuffer(..), Sound(..), play, createSound, setSoundBuffer, SFSound (setVolume))
import SFML.Audio.SoundBuffer (soundBufferFromFile)
import SFML.SFResource (SFResource(..))

import CPU (CPU(..))
import Expected (Expected(..), fromEither, liftIO, isUnexpected, expected)
import Paths_Chip8 -- assets, generated by Cabal
import Debug.Trace (traceShow)

data Sound = Sound {
    buffer :: SFML.Audio.SoundBuffer,
    sound :: SFML.Audio.Sound
}

instance Show Sound where
    show :: Sound -> String
    show _ = "Beep sound"

instance SFResource Sound where
    destroy :: Sound -> IO ()
    destroy sound' = destroy (sound sound') >> destroy (buffer sound')

makeSound :: IO (Expected Sound)
makeSound = do
    buffer' <- fmap fromEither (getDataFileName "beep.wav" >>= soundBufferFromFile)
    sound' <- SFML.Audio.createSound
    SFML.Audio.setSoundBuffer sound' (expected buffer')
    return $ Expected (Sound (expected buffer') sound')

-- showExpected :: Expected a -> String
-- showExpected (Expected _) = "Expected"
-- showExpected _ = "Unexpected"

-- traceExpected :: Expected a -> Expected a
-- traceExpected a = traceShow (showExpected a) a

-- here, no sound because buffer' is evaluated twice, thus the sound buffer is replaced by a brand new one, but the sound isn't linked with the new
-- makeSound = liftA2 (liftA2 Sound) buffer' sound'
    -- where buffer' = fmap (traceExpected . fromEither) (getDataFileName "beep.wav" >>= soundBufferFromFile)
        --   sound' = liftA2 (\sound'' buffer'' -> fmap (\b -> SFML.Audio.setSoundBuffer sound'' b >> SFML.Audio.play sound'') buffer'' >> return sound'') SFML.Audio.createSound buffer'

playSoundIfCounterOk :: CPU -> Sound -> CPU
playSoundIfCounterOk cpu' sound'
    | (soundCounter cpu') == 0 = cpu'
    | otherwise = unsafePerformIO $! SFML.Audio.play (sound sound') >> return cpu' { soundCounter = 0 }