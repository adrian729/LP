data BST a = E | N a (BST a) (BST a) deriving (Show)

--insert :: Ord a => BST a -> a -> BST a
--Retorna el resultat d’inserir un element en un arbre binari de cerca. Si l’element ja hi era, el resultat és l’arbre original.
insert :: Ord a => BST a -> a -> BST a
insert E x = N x E E
insert (N y l r) x
    | x == y    = N x l r
    | x < y     = N y (insert l x) r
    | otherwise = N y l (insert r x)

--create :: Ord a => [a] -> BST a
--Retorna un arbre binari de cerca inserint l’un rera l’altre la llista d’elements donats.

--remove :: Ord a => BST a -> a -> BST a
--Retorna el resultat d’esborrar un element d’un arbre binari de cerca. Si l’element no hi era, el resultat és l’arbre original.
--(Nota: hi ha moltes maneres d’implementar l’esborrat; no importa quina trieu mentre sigui prou ràpida.)

--contains :: Ord a => BST a -> a -> Bool
--Indica si un element es troba o no en un arbre binari de cerca.

--getmax :: BST a -> a
--Retorna l’element més gran d’un arbre binari de cerca no buit.

--getmin :: BST a -> a
--Retorna l’element més petit d’un arbre binari de cerca no buit.

--size :: BST a -> Int
--Retorna el nombre d’elements en un arbre binari de cerca.

--elements :: BST a -> [a]
--Retorna els elements d’un arbre binari de cerca en ordre.