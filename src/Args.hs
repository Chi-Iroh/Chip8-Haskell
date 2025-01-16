module Args (readROM) where

import Data.Functor ((<&>))
import System.Environment (getArgs)

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as Byte (readFile, unpack)

import CPU (CPU(..), loadROM)
import Expected (Expected(..))

getROMName :: [String] -> Expected FilePath
getROMName [arg] = Expected arg
getROMName args = Unexpected ("Got " ++ show (length args) ++ "arg(s) but only 1 expected !")

readROM :: IO (Expected CPU)
readROM = getArgs >>= (Byte.readFile . head) <&> (loadROM . Byte.unpack)
