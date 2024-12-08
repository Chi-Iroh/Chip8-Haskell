module Hex (Word8(..), Word16(..), showHex8, showHex16) where

import Word (Word8(..), Word16(..), int, u8to16)

showHex' :: Word16 -> String
showHex' u16
    | u16 < 0x10 = ["0123456789ABCDEF" !! int u16]
    | otherwise = showHex' (rem u16 0x10) ++ showHex' (div u16 0x10)

showHex16 :: Word16 -> String
showHex16 = ("0x" ++) . reverse . showHex'

showHex8 :: Word8 -> String
showHex8 = showHex16 . u8to16