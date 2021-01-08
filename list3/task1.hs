data Tree a = Leaf a | Node (Tree a) a (Tree a)

instance (Show a) => Show (Tree a) where
    show (Leaf a) = show a
    show (Node l a r) = show l ++ " " ++ show r ++ " " ++ show a -- nie działa

instance Foldable Tree where
    foldr f x (Leaf a) = f a x
    foldr f x (Node l a r) = foldr f (f a (foldr f x r)) l

height (Leaf a) = 0
height (Node l a r) = 1 + (max (height l) (height r))

roots (Leaf x) = 0
roots (Node l a r) = 1 + (roots l) + (roots r)

leaves (Leaf x) = 1
leaves (Node l a r) = leaves l + leaves r

contains x (Leaf a) = if (x == a) then True else False
contains x (Node l a r) = (x == a) || contains x l || contains x r


