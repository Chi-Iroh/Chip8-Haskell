{-# LANGUAGE InstanceSigs #-}

module CPU (Word8, CPU(..), loadROM, memorySize, memoryStart, isValidPc, checkPc, incrementPc, decrementCounters) where

import Text.Printf (printf)

import Expected (Expected(..))
import Font (initFont)
import Hex (showHex16, showHex8)
import Word (u8, u16, int, Word8, Word16)

memorySize :: Word16
memorySize = 4096

memoryStart :: Word16
memoryStart = 512

romSize :: Word16
romSize = memorySize - memoryStart

nRegisters :: Int
nRegisters = 16

data CPU = CPU {
    memory :: [Word8],
    pc :: Word16,
    v :: [Word8],
    i :: Word16,
    jumps :: [Word16],
    soundCounter :: Word8,
    sysCounter :: Word8
}

decrementCounter :: Word8 -> Word8
decrementCounter = const 0

decrementCounters :: CPU -> CPU
decrementCounters cpu = cpu {
    soundCounter = decrementCounter (soundCounter cpu),
    sysCounter = decrementCounter (sysCounter cpu)
}

initInterpreterData :: [Word8]
initInterpreterData = font ++ replicate (int memoryStart - length font) 0
    where font = initFont

loadROM' :: [Word8] -> [Word8]
loadROM' rom = initInterpreterData ++ rom ++ replicate (int romSize - romSize') 0
    where romSize' = length rom

loadROM :: [Word8] -> Expected CPU
loadROM rom
    | romSize' > int romSize = Unexpected ("ROM too large, weights " ++ show romSize' ++ " bytes instead of at most " ++ show romSize ++ " bytes !")
    | otherwise = Expected CPU {
        memory = loadROM' rom,
        pc = memoryStart,
        v = replicate nRegisters 0,
        i = 0,
        jumps = [],
        soundCounter = 0,
        sysCounter = 0
    }
    where romSize' = length rom

inRange :: Ord a => a -> a -> a -> Bool
inRange start end a = start <= a && a <= end

isValidPc :: Word16 -> Bool
isValidPc = inRange (memoryStart - 2) (memorySize - 2)

checkPc :: Word16 -> Expected Word16
checkPc pc'
    | isValidPc pc' = Expected pc'
    | otherwise = Unexpected ("PC out of range ! Must be between " ++ showHex16 memoryStart ++ " and " ++ showHex16 (memorySize - 2) ++ " but is " ++ showHex16 pc' ++ " !")

incrementPc :: CPU -> Expected CPU
incrementPc cpu = checkPc (pc cpu) >> checkPc (pc cpu + 2) >>= (\pc' -> Expected cpu { pc = pc' })