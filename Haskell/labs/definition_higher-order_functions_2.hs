--Define a function countIf :: (Int -> Bool) -> [Int] -> Int that,
--given a predicate on integers and a list of integers, returns the
--number of elements in the list that satify the predicate.
countIf :: (Int -> Bool) -> [Int] -> Int
countIf f = foldr (\x y -> satisfy f x + y) 0

satisfy :: (Int -> Bool) -> Int -> Int
satisfy f n
    | f n       = 1
    | otherwise = 0
--countIf f x 	= length (filter f x)

--Define a function pam :: [Int] -> [Int -> Int] -> [[Int]] that, 
--given a list of integers and a list of functions from integers to 
--integers, returns the list consisting of applying each of the 
--functions in the second list to the elements in the first list.
pam :: [Int] -> [Int -> Int] -> [[Int]]
pam li = foldr (\f y -> [map f li] ++ y) []
--pam l f = map (\fx -> map fx l) f

--Define a function pam2 :: [Int] -> [Int -> Int] -> [[Int]] that,
--given a list of integers and a list of functions from integers to
--integers, returns the list of lists where each list is the result
--of applying, one after the other, the function in the second list
--to each element in the first list.
pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 li lf = foldr (\i y -> [[f i | f <- lf]] ++ y) [] li
--pam2 l f = map (\x -> map (\fx -> fx x) f) l

--Define a function 
--filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
--that returns a fold of all the elements that satisfy the given predicate.
filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl fb f n li = foldl f n (filter fb li)
--filterFoldl _ _ a [] = a
--filterFoldl fb f a (b:sb)
--    | fb b      = filterFoldl fb f (f a b) sb
--    | otherwise = filterFoldl fb f a sb

--Define a function insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] that,
--given a relation between integers, a list and un element, return
--the list with the inserted element according to the relation.
insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert _ [] n = [n]
insert f (e:se) n
    | f e n     = e:(insert f se n) 
    | otherwise = n:(e:se)
--insert f l n = takeWhile (\x -> (f x n)) l ++ [n] ++ dropWhile (\x -> (f x n)) l

--Use function insert, in order to define function 
--insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] that orders
--a list according to the given relation.
insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f l = foldr (\x xs -> insert f xs x) [] l
