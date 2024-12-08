module CPU (Word8, CPU(..), loadROM) where

import Data.Word (Word8, Word16)

import Expected (Expected(..))

int :: Enum a => a -> Int
int = fromEnum

u8 :: Int -> Word8
u8 = toEnum

u16 :: Int -> Word16
u16 = toEnum

memorySize :: Int
memorySize = 4096

memoryStart :: Int
memoryStart = 512

romSize :: Int
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
loadROM' rom = replicate memoryStart 0 ++ rom ++ replicate (romSize - romSize') 0
    where romSize' = length rom

loadROM :: [Word8] -> Expected CPU
loadROM rom
    | romSize' > romSize = Unexpected ("ROM too large, weights " ++ show romSize' ++ " bytes instead of at most " ++ show romSize ++ " bytes !")
    | otherwise = Expected CPU {
        memory = loadROM' rom,
        pc = u16 memoryStart,
        v = replicate nRegisters 0,
        i = 0,
        jumps = [],
        soundCounter = 0,
        sysCounter = 0
    }
    where romSize' = length rom
