move pos step
  | (2>=(pos+step)) && ((pos+step)>=(-2)) = Just (pos+step)
  | otherwise = Nothing
  
move_list (x:xs) pos
  | length xs == 0 = Just (pos+x)
  | (2>=(pos+x)) && ((pos+x)>=(-2)) = move_list xs (pos+x)
  | otherwise = Nothing