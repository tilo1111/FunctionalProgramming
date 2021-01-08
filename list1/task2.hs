euler n = length [x | x <- [1..n], gcd x n == 1]

sumDiv n = sum [euler k | k <- [1..n], n `mod` k == 0]