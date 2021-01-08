data Tree a = Leaf a | Node a [Tree a]

instance (Show a) => Show (Tree a) where
    show (Leaf a) = show a
    show (Node a (x:xs)) = show a  -- nie działa 

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a x) = Node (f a) (fmap (fmap f) x)

instance Foldable Tree where
    foldr f y (Leaf a) = f a y
    foldr f y (Node a []) = f a y
    foldr f y (Node a (x:xs)) = foldr f (foldr f y x) (Node a xs)

roots (Leaf a) = 0
roots (Node a []) = 1
roots (Node a (x:xs)) = roots x + roots (Node a xs)

leaves (Leaf x) = 1
leaves (Node a []) = 0
leaves (Node a (x:xs)) = leaves x + leaves (Node a xs)

contains y (Leaf a) = if (y == a) then True else False
contains y (Node a []) = if (y == a) then True else False
contains y (Node a (x:xs)) = (y == a) || contains y x || contains y (Node a xs)