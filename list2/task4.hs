approx_e n = sum (foldl (\xs x -> ((head xs)/x) :xs) [1] [1..n])
