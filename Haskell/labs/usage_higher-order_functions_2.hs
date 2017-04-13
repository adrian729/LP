--Implement a function flatten :: [[Int]] -> [Int] that flattens a list of lists of integers in a list of integers.
flatten :: [[Int]] -> [Int]
flatten = foldr (++) []

--Implement a function myLength :: String -> Int that returns the length of a string.
myLength :: String -> Int
myLength = foldr (\x y -> y + 1) 0

--Implement a function myReverse :: [Int] -> [Int] that reverses a list of integers.
myReverse :: [Int] -> [Int]
myReverse = foldr (\x y -> y ++ [x]) []

--Implement a function countIn :: [[Int]] -> Int -> [Int] that, given a list of sublists ℓ and an element x, returns the list that tells how many times x appears in each sublist of ℓ.
countIn :: [[Int]] -> Int -> [Int]
countIn l n = map (\x -> length (filter (== n) x)) l

--Implement a function firstWord :: String -> String that, given a string with blanks and alphabetic characters, returns its first word.
firstWord :: String -> String
firstWord w = takeWhile (/= ' ') (dropWhile (== ' ') w)