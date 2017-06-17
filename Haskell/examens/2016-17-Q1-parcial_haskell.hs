-- Problema 1
mergelist :: Ord a => [[a]] -> [a]
mergelist = foldr mergeSortedSets []

-- Merge ordenat de conjunts ordenats
mergeSortedSets :: Ord a => [a] -> [a] -> [a]
mergeSortedSets [] l2 = l2
mergeSortedSets l1 [] = l1
mergeSortedSets l1@(e1:se1) l2@(e2:se2)
    | e1 == e2  = e1:(mergeSortedSets se1 se2)
    | e1 < e2   = e1:(mergeSortedSets se1 l2)
    | otherwise = e2:(mergeSortedSets l1 se2)

-- Problema 2
mults :: [Integer] -> [Integer]
mults n = 1 : (mergelist $ map multsN n)

multsN :: Integer -> [Integer]
multsN n = map (*n) [1..]

-- Problema 3.1
data Procs a =
    End
    | Skip (Procs a)
    | Unary (a -> a) (Procs a)
    | Binary (a -> a -> a) (Procs a)

-- Problema 3.2
exec :: [a] -> (Procs a) -> [a]
exec [] _ = []
exec l End = l
exec (e:se) (Skip sp) = e : (exec se sp)
exec (e:se) (Unary f sp) = exec (f e : se) sp
exec [e] (Binary f sp) = exec [f e e] sp
exec (e1:(e2:se)) (Binary f sp) = exec (f e1 e2 : se) sp

-- Problema 4.1
class Container c where
    emptyC :: c a -> Bool
    lengthC :: c a -> Int
    firstC :: c a -> a
    popC :: c a -> c a

-- Problema 4.2
instance Container [] where
    emptyC [] = True
    emptyC _ = False
    lengthC l = length l
    firstC (e:se) = e
    popC (e:se) = se

-- Problema 4.3
data Tree a =
    Empty
    | Node a [Tree a]

-- Problema 4.4
instance Container Tree where
    emptyC Empty = True
    emptyC _ = False
    lengthC Empty = 0
    lengthC (Node a l) = foldr ((+) . lengthC) 1 l
    firstC (Node a _) = a
    popC (Node a []) = Empty
    popC (Node a (Empty:se)) = popC (Node a se)
    popC (Node a (e:se)) = Node b $ bl ++ se
        where Node b bl = e

-- Problema 4.5
iterator :: Container c => c a -> [a]
iterator t
    | emptyC t   = []
    | otherwise  = [firstC t] ++ (iterator $ popC t)