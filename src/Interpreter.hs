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

data Interpreter = Interpreter {
    cpu :: CPU,
    screen :: Screen,
    window :: RenderWindow,
    seed :: StdGen
}

instance Show Interpreter where -- for debug purpose
    show :: Interpreter -> String
    show interpreter = "Interpreter { cpu = " ++ show (cpu interpreter) ++ " }"

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = liftA3 f a b c <*> d

makeInterpreter :: Expected Interpreter
makeInterpreter = liftA4 Interpreter cpu' screen' window' seed'
    where cpu' = readROM
          screen' = makeScreen
          window' = makeWindow
          seed' = getStdGen :: Expected StdGen

destroyInterpreter :: Interpreter -> Expected ()
destroyInterpreter interpreter = destroyAll [SFMLResource (window interpreter), SFMLResource (screen interpreter)]