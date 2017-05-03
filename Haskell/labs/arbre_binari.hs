data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

--Feu una funció size :: Tree a -> Int que, donat un arbre, retorni la seva talla, és a dir, el nombre de nodes que conté.
size :: Tree a -> Int
size Empty = 0
size (Node a b c) = 1 + size b + size c

--Feu una funció height :: Tree a -> Int que, donat un arbre, retorni la seva alçada, assumint que els arbres buits tenen alçada zero.
height :: Tree a -> Int
height Empty = 0;
height (Node a b c) = 1 + max (height b) (height c)

--Feu una funció equal :: Eq a => Tree a -> Tree a -> Bool que, donat dos arbres, indiqui si són el mateix.
instance Eq a => Eq (Tree a) where
    Empty == Empty                   = True
    _     == Empty                   = False
    Empty == _                       = False
    (Node x a1 a2) == (Node y b1 b2) = x == y && a1 == b1 && a2 == b2

equal :: Eq a => Tree a -> Tree a -> Bool
equal a b = a == b

--Feu una funció isomorphic :: Eq a => Tree a -> Tree a -> Bool que, donat un arbres, indiqui si són el isomorfs, és a dir, si es pot obtenir l’un de l’altre tot girant algun dels seus fills.
isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _     = False
isomorphic _     Empty = False
isomorphic (Node x a1 a2) (Node y b1 b2)
    | x /= y    = False
    | otherwise = (isomorphic a1 b1 && isomorphic a2 b2) 
                  || (isomorphic a1 b2 && isomorphic a2 b1)

--Feu una funció preOrder :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut en pre-ordre.
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x a b) = [x] ++ preOrder a ++ preOrder b

--Feu una funció postOrder :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut en post-ordre.
postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x a b) = postOrder a ++ postOrder b ++ [x]

--Feu una funció inOrder :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut en in-ordre.
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x a b) = inOrder a ++ [x] ++ inOrder b

--Feu una funció breadthFirst :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut per nivells.
breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst (Node x t1 t2) = breadthFirst' (Node x Empty (Node x t1 t2))

breadthFirst' :: Tree a -> [a]
breadthFirst' Empty = []
breadthFirst' (Node _ Empty Empty) = []
breadthFirst' (Node _ Empty (Node x t1 t2)) = 
    [x] ++ breadthFirst' (Node x t1 t2)
breadthFirst' (Node _ (Node x t1 t2) Empty) = 
    [x] ++ breadthFirst' (Node x t1 t2)
breadthFirst' (Node _ (Node x t1 t2) (Node y t3 t4)) =
    [x, y] ++ breadthFirst' (Node x t1 t2) ++ breadthFirst' (Node y t3 t4)


--Feu una funció build :: Eq a => [a] -> [a] -> Tree a que, donat el recorregut en pre-ordre d’un arbre i el recorregut en in-ordre del mateix arbre, retorni l’arbre original. Assumiu que l’arbre no té elements repetits.
build :: Eq a => [a] -> [a] -> Tree a
build [] _ = Empty
build (e:se) l2
    | null lli && null rli = Node e Empty Empty
    | null rli             = Node e (build llp lli) Empty
    | null lli             = Node e Empty (build rlp rli)
    | otherwise            = Node e (build llp lli) (build rlp rli)
    where (lli, rli) = splitAtElem (==) e ([], l2)--Part esquerra i dreta inordre
          (llp, rlp) = splitAt (length lli) se--Part esquerra i dreta preordre

splitAtElem :: (a -> a -> Bool) -> a -> ([a],[a]) -> ([a],[a])
splitAtElem _ _ (l,[]) = (l,[])
splitAtElem f n (l,(e:se))
    | f n e     = (l, se)
    | otherwise = splitAtElem f n (l ++ [e], se)

--Feu una funció overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a que, donats dos arbres, retorni la seva superposició utilitzant una funció. Superposar dos arbres amb una funció consisteix en posar els dos arbres l’un damunt de l’altre i combinar els nodes doble resultants amb la funció donada o deixant els nodes simples tal qual.
overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap f n Empty = n
overlap f Empty n = n
overlap f (Node x a1 b1) (Node y a2 b2) = Node (f x y) (overlap f a1 a2) (overlap f b1 b2)