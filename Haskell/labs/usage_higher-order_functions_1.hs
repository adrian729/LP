--Implement a function eql :: [Int] -> [Int] -> Bool that tells wether two lists of integers are equal.
eql :: [Int] -> [Int] -> Bool
eql l1 l2 = l1 == l2

--Implement a function prod :: [Int] -> Int that returns the product of a list of integers.
--Tambe es pot fer amb prod = product, que ja esta implementada i fa el mateix...
prod :: [Int] -> Int
prod = foldr (*) 1

--Implement a function prodOfEvens :: [Int] -> Int that returns the product of all even numbers of a list of integers.
prodOfEvens :: [Int] -> Int
prodOfEvens l = prod (filter even l)

--Implement a function powersOf2 :: [Int] that generates the list of all the powers of 2.
powersOf2 :: [Int]
powersOf2 = zipWith (^) [2,2..] [0..]

--Implement a function scalarProduct :: [Float] -> [Float] -> Float that returns the dot product of two lists of float numbers with the same size.
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = foldr (+) 0 (zipWith (*) l1 l2)