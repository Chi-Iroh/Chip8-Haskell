{-# LANGUAGE InstanceSigs #-}
module Interpreter (Interpreter(..), module Screen, module CPU, makeInterpreter, destroyInterpreter) where

import Control.Applicative (liftA3)
import System.Exit (die)
import System.Random (StdGen, getStdGen)

import SFML.Graphics (RenderWindow)

import Args (readROM)
import CPU (CPU)
import Destroy (SFMLResource(..), destroyAll)
import Expected (Expected(..))
import Screen (Screen, makeScreen, makeWindow)
import Sound
import Sound (makeSound)

data Interpreter = Interpreter {
    cpu :: CPU,
    screen :: Screen,
    window :: RenderWindow,
    seed :: StdGen,
    beep :: Sound
}

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d e = liftA3 f a b c <*> d <*> e

makeInterpreter :: IO (Expected Interpreter)
makeInterpreter = liftA5 (liftA5 Interpreter) cpu' screen' window' seed' sound'
    where cpu' = readROM
          screen' = makeScreen
          window' = makeWindow
          rawSeed = getStdGen :: IO StdGen
          seed' = Expected <$> rawSeed
          sound' = makeSound

destroyInterpreter :: Expected Interpreter -> IO ()
destroyInterpreter (Unexpected err) = die err
destroyInterpreter (Expected interpreter) = destroyAll [SFMLResource (window interpreter), SFMLResource (screen interpreter), SFMLResource (beep interpreter)]