module Args (readROM) where

import Control.Exception (try)
import Control.Monad (join)
import System.Environment (getArgs)

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as Byte (readFile, unpack)

import CPU (CPU(..), loadROM)
import Expected (Expected(..))

getROMName :: [String] -> Expected FilePath
getROMName [arg] = Expected arg
getROMName args = Unexpected ("Got " ++ show (length args) ++ "arg(s) but only 1 expected !")

readROM :: IO (Expected CPU)
readROM = fmap (loadROM . Byte.unpack) (fmap head getArgs >>= Byte.readFile)
