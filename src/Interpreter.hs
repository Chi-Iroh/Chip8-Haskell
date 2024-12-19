{-# LANGUAGE InstanceSigs #-}
module Interpreter (Interpreter(..), module Screen, module CPU, makeInterpreter, destroyInterpreter) where

import Control.Applicative (liftA3)
import System.Random (StdGen, getStdGen)

import SFML.Graphics (RenderWindow)

import Args (readROM)
import CPU (CPU)
import Destroy (SFMLResource(..), destroyAll)
import Expected (Expected(..))
import Screen (Screen, makeScreen, makeWindow)
import Sound

data Interpreter = Interpreter {
    cpu :: CPU,
    screen :: Screen,
    window :: RenderWindow,
    seed :: StdGen,
    beep :: Sound
}

instance Show Interpreter where -- for debug purpose
    show :: Interpreter -> String
    show interpreter = "Interpreter { cpu = " ++ show (cpu interpreter) ++ " }"

liftA5 :: Applicative f => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d e = liftA3 f a b c <*> d <*> e

makeInterpreter :: IO (Expected Interpreter)
makeInterpreter = fmap (liftA5 Interpreter cpu' screen' window' seed') makeSound
    where cpu' = readROM
          screen' = makeScreen
          window' = makeWindow
          seed' = getStdGen :: Expected StdGen

destroyInterpreter :: Interpreter -> Expected ()
destroyInterpreter interpreter = destroyAll [SFMLResource (window interpreter), SFMLResource (screen interpreter), SFMLResource (beep interpreter)]