--Define a function insert :: [Int] -> Int -> [Int] that, given a sorted list and an element, correctly inserts the new element in the list.
insert :: [Int] -> Int -> [Int]
insert [] n = [n]
insert (e:se) n
    | e < n     = e : (insert se n)
    | otherwise = n : (e:se)

--Define a function isort :: [Int] -> [Int] that implements insertion sort using the previous function.
isort :: [Int] -> [Int]
isort [] = []
isort (e:se) = insert (isort se) e

--Define a function remove :: [Int] -> Int -> [Int] that, given a list and an element x, erases the first occurrence of x from the list. You can assume that the element is always in the list.
remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (e:se) x
    | e == x    = se
    | otherwise = e:(remove se x)

--Define a function ssort :: [Int] -> [Int] that implements selection sort using the previous function.
ssort :: [Int] -> [Int]
ssort [] = []
ssort l = mn : (ssort (remove l mn))
    where mn = minList l

minList :: [Int] -> Int
minList [e] = e
minList (e:se)
    | mn < e    = mn
    | otherwise = e
    where mn = minList se

--Define a function merge :: [Int] -> [Int] -> [Int] that, given two sorted lists, merges them to get a list with all the elements in sorted order.
merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l [] = l
merge (e1:se1) (e2:se2)
    | e1 < e2   = e1 : (merge se1 (e2:se2))
    | otherwise = e2 : (merge (e1:se1) se2)

--Define a function msort :: [Int] -> [Int] that implements merge sort using the previous function.
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort l = merge (msort ll) (msort lr)
    where splitedList = splitAt (div (length l) 2) l
          ll = fst splitedList
          lr = snd splitedList

--Define a function qsort :: [Int] -> [Int] that implements quick sort.
qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (e:se) = qsort ll ++ [e] ++ qsort lr
    where l = qsort' e se
          ll = fst l
          lr = snd l

qsort' :: Int -> [Int] -> ([Int], [Int])
qsort' _ [] = ([], [])
qsort' x (e:se)
    | e <= x    = (e:l1, l2)
    | otherwise = (l1, e:l2)
    where (l1, l2) = qsort' x se

--Generalize the previous function into genQsort :: Ord a => [a] -> [a] that sorts elements of any type.
genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort [x] = [x]
genQsort (e:se) = genQsort ll ++ [e] ++ genQsort lr
    where l = genQsort' e se
          ll = fst l
          lr = snd l

genQsort' :: Ord a => a -> [a] -> ([a], [a])
genQsort' _ [] = ([], [])
genQsort' x (e:se)
    | e <= x    = (e:l1, l2)
    | otherwise = (l1, e:l2)
    where (l1, l2) = genQsort' x se