{-# LANGUAGE InstanceSigs #-}
module Expected (Expected(..), fromEither, isExpected, isUnexpected, expected, unexpected, fromMaybe) where

import Control.Exception (Exception)
import System.IO.Unsafe (unsafePerformIO)

data Expected a = Expected a | Unexpected String deriving Eq

instance Functor Expected where
    fmap :: (a -> b) -> Expected a -> Expected b
    fmap f (Expected a) = Expected (f a)
    fmap _ (Unexpected err) = Unexpected err

instance Applicative Expected where
    pure :: a -> Expected a
    pure = Expected

    (<*>) :: Expected (a -> b) -> Expected a -> Expected b
    (Expected f) <*> (Expected a) = Expected (f a)
    (Unexpected err) <*> _ = Unexpected err
    _ <*> (Unexpected err) = Unexpected err

    (*>) :: Expected a -> Expected b -> Expected b
    (Expected _) *> a = a
    (Unexpected err) *> _ = Unexpected err

instance Monad Expected where
    (>>=) :: Expected a -> (a -> Expected b) -> Expected b
    (Expected a) >>= f = f a
    (Unexpected err) >>= _ = Unexpected err

    (>>) :: Expected a -> Expected b -> Expected b
    (>>) = (*>)

    return :: a -> Expected a
    return = pure

instance Show a => Show (Expected a) where
    show :: Show a => Expected a -> String
    show (Unexpected err) = err
    show (Expected a) = show a

isExpected :: Expected a -> Bool
isExpected (Expected _) = True
isExpected _ = False

isUnexpected :: Expected a -> Bool
isUnexpected (Unexpected _) = True
isUnexpected _ = False

fromEither :: Exception e => Either e a -> Expected a
fromEither (Left error) = Unexpected (show error)
fromEither (Right a) = Expected a

fromMaybe :: String -> Maybe a -> Expected a
fromMaybe _ (Just a) = Expected a
fromMaybe errorMessage Nothing = Unexpected errorMessage

unexpected :: Expected a -> String
unexpected (Unexpected err) = err
unexpected _ = error "Got Expected value instead of Unexpected !"

expected :: Expected a -> a
expected (Expected a) = a
expected _ = error "Got Unexpected instead of Expected value !"