import Data.List

ecd s = f s [] where
  f (x:y:xs) a
    | x == y && xs == [] = (a++[x])
    | x == y && xs /= [] = f (y:xs) a
    | x /= y && xs == [] = (a++[x]++[y])
    | x /= y && xs /= [] = f (y:xs) (a++[x])

encode s = [(length x, head x) | x <- group s]

decode s = f s [] where
  f ((x,y):xs) a
    | xs /= [] = f xs (a++(take x (repeat y)))
    | xs == [] = (a++(take x (repeat y)))