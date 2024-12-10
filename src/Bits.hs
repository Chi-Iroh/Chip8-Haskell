module Bits (toBits) where

import Data.Bits (FiniteBits (finiteBitSize), Bits (testBit))

toBits :: FiniteBits a => a -> [Bool]
toBits a = map (testBit a) (reverse [0..(finiteBitSize a - 1)])