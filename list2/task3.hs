filter1 p (x:xs)
    | p x       = x : filter1 p xs
    | otherwise = filter1 p xs
filter1 _ [] = []

filter2 p xs = foldr (\x xs -> if p x then x : xs else xs) [] xs
