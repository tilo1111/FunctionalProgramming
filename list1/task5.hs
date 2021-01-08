import Data.List

power_list [] = [[]]
power_list (x:xs) = [x:sub | sub <- power_list xs] ++ power_list xs

perm s = permutations s