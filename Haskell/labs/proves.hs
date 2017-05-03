data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

instance Eq a => Eq (Tree a) where
    Empty == Empty                   = True
    _     == Empty                   = False
    Empty == _                       = False
    (Node x a1 a2) == (Node y b1 b2) = x == y && a1 == b1 && a2 == b2