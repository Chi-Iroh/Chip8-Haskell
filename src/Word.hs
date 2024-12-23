module Word (
    u8,
    u16,
    u8to16,
    u16to8,
    int,
    addOverflow8,
    subUnderflow8,
    Word8(..),
    Word16(..)
    ) where

import Data.Word (Word8(..), Word16(..))

int :: Enum a => a -> Int
int = fromEnum

u8 :: Int -> Word8
u8 = toEnum

u16 :: Int -> Word16
u16 = toEnum

u8to16 :: Word8 -> Word16
u8to16 = u16 . int

u16to8 :: Word16 -> Word8
u16to8 = u8 . int

addOverflow8 :: Word8 -> Word8 -> (Word8, Bool)
addOverflow8 a b = (a + b, b > (0xFF - a))

subUnderflow8 :: Word8 -> Word8 -> (Word8, Bool)
subUnderflow8 a b = (a - b, a < b)