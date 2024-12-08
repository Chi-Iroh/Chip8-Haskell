module CPU (Word8, CPU(..), loadROM, memorySize, memoryStart) where

import Expected (Expected(..))
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
} deriving Show

decrementCounter :: Word8 -> Word8
decrementCounter n = if n == 0 then 0 else n - 1

decrementCounters :: CPU -> CPU
decrementCounters cpu = cpu {
    soundCounter = decrementCounter (soundCounter cpu),
    sysCounter = decrementCounter (sysCounter cpu)
}

loadROM' :: [Word8] -> [Word8]
loadROM' rom = replicate (int memoryStart) 0 ++ rom ++ replicate (int romSize - romSize') 0
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
