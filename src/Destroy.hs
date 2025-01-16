{-# LANGUAGE ExistentialQuantification #-}
module Destroy (SFMLResource(..), SFResource(..), destroyAll) where

import SFML.SFResource (SFResource(..), destroy)

data SFMLResource = forall a. SFResource a => SFMLResource a

destroy' :: SFMLResource -> IO ()
destroy' (SFMLResource a) = destroy a

destroyAll :: [SFMLResource] -> IO ()
destroyAll = mapM_ destroy'