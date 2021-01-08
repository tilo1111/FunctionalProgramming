rev [] = []
rev (x:xs) = rev xs ++ [x]

rev_rev [] = []
rev_rev (x:xs) = rev_rev xs ++ [rev x]