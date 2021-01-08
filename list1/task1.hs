import Data.List

sieve n = f [2..n] [] where
  f (x:[]) a = (a++[x])
  f (x:xs) a = f (xs \\ [x+x, x+x+x..n]) (a++[x])