module Args (readROM) where

import Data.Functor ((<&>))
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as Byte (readFile, unpack)

import CPU (CPU(..), loadROM)
import Expected (Expected(..))

helpMsg :: String
helpMsg = "hUsage: cabal run Chip8 -- rom_file"

getROMName :: [String] -> Expected FilePath
getROMName [] = Unexpected helpMsg
getROMName ("--help" : _) = Unexpected helpMsg
getROMName ("-h" : _) = Unexpected helpMsg
getROMName [arg] = Expected arg
getROMName args = Unexpected ("Got " ++ show (length args) ++ " arg(s) but only 1 expected !")

readROM :: IO (Expected CPU)
readROM = do
    args <- getArgs
    case getROMName args of
        (Unexpected ('h' : err)) -> putStrLn err >> exitSuccess
        (Unexpected err) -> die err
        (Expected filepath) -> (Byte.readFile filepath) <&> (loadROM . Byte.unpack)
