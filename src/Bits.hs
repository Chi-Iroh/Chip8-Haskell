module Bits ((.>>), (.<<)) where

import Data.Bits (Bits(..), shiftR, shiftL)

infixl 5 .>>    -- left associative, as (100 >> 1) >> 2 == (100 >> 1) >> 2
                -- priority 5 to be just less tightly than + and -, like in C++
(.>>) :: Bits a => a -> Int -> a
(.>>) = shiftR

infixl 5 .<<    -- left associative, as 1 << 2 << 3 == (1 << 2) << 3
                -- priority 5 to be just less tightly than + and -, like in C++
(.<<) :: Bits a => a -> Int -> a
(.<<) = shiftL