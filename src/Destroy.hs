{-# LANGUAGE ExistentialQuantification #-}
module Destroy (SFMLResource(..), destroyAll) where

import SFML.SFResource (SFResource(..), destroy)
import Expected (Expected(..), liftIO)

data SFMLResource = forall a. SFResource a => SFMLResource a

destroy' :: SFMLResource -> Expected ()
destroy' (SFMLResource a) = liftIO (destroy a)

destroyAll :: [SFMLResource] -> Expected ()
destroyAll = mapM_ destroy'