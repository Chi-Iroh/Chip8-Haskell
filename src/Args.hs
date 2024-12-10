module Args (readROM) where

import System.Environment (getArgs)

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as Byte (readFile, unpack)

import CPU (CPU(..), loadROM)
import Expected (Expected(..), liftIO)

getROMName :: [String] -> Expected FilePath
getROMName [arg] = Expected arg
getROMName args = Unexpected ("Got " ++ show (length args) ++ "arg(s) but only 1 expected !")

readROM :: Expected CPU
readROM = liftIO (getArgs >>= (Byte.readFile . head)) >>= (loadROM . Byte.unpack)
