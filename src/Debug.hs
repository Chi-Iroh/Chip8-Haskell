module Debug (debug, debug2, debugMap) where

import Debug.Trace (trace)

-- debug "Hello " 4 5 prints "Hello 4" and returns 5
debug2 :: Show a => String -> a -> b -> b
debug2 msg a = trace (msg ++ show a)

-- debug "Hello " 4 prints "Hello 4" and returns 4
debug :: Show a => String -> a -> a
debug msg a = debug2 msg a a

-- debugMap "Hello " 4 (+ 1) printf "Hello 5" and returns 4
debugMap :: Show b => String -> (a -> b) -> a -> a
debugMap msg f a = debug2 msg (f a) a
