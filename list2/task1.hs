mapr f xs = foldr (\y ys -> (f y):ys) [] xs
mapl f xs = foldl (\ys y -> ys ++ [f y]) [] xs