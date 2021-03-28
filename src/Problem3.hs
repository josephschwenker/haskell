module Problem3 where

type Delt a = [a] -> [a]

nil :: Delt a
nil xs = xs