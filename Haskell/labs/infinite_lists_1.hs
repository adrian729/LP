--Generate the sequence of ones [1,1,1,1,1,1,1,1,…].
ones :: [Integer]
ones = iterate (+0) 1

--Generate the sequence of the natural numbers [0,1,2,3,4,5,6,7…].
nats :: [Integer]
nats = iterate (+1) 0

--Generate the sequence of the integer numbers [0,1,−1,2,−2,3,−3,4…].
ints :: [Integer]
ints = 0:foldr (\x y -> x:[-x] ++ y) [] (iterate (+1) 1)
--ne :: Integer -> Integer
--ne 0 = 1
--ne x
--    | x < 0     = -1*x + 1
--    | otherwise = -1*x
-- ints = iterate ne 0

--Generate the sequence of the triangular numbers: 0,1,3,6,10,15,21,28,…].
triangulars :: [Integer]
triangulars = scanl (+) 0 $ iterate (+1) 1
--triangulars = map (\x -> x*(x+1) `div` 2) nats

--Generate the sequence of the factorial numbers: [1,1,2,6,24,120,720,5040,…].
factorials :: [Integer]
factorials = scanl (*) 1 (iterate (+1) 1)

--Generate the sequence of the Fibonacci numbers: [0,1,1,2,3,5,8,13,…].
fibs :: [Integer]
fibs = 0:1:fibs' 0 1

fibs' :: Integer -> Integer -> [Integer]
fibs' n1 n2 = n3 : fibs' n2 n3
    where n3 = n1 + n2

--Generate the sequence of prime numbers: [2,3,5,7,11,13,17,19,…].
primes :: [Integer]
primes = primes' [] 2

primes' :: [Integer] -> Integer -> [Integer]
primes' l n
    | primeNum l n = n : primes' (l ++ [n]) (n+1)
    | otherwise    = primes' l (n+1)

primeNum :: [Integer] -> Integer -> Bool
primeNum l n = and $ map (\x -> mod n x /= 0) $ takeWhile (\x -> x*x <= n) $ l

--primes = lprimes $ iterate (+1) 2
--    where lprimes (x:xs) = x:(lprimes $ filter (\y -> (mod y x)/=0) xs)

--Generate the ordered sequence of the Hamming numbers: [1,2,3,4,5,6,8,9,…].
--The Hamming numbers are those that only have 2, 3 and 5 as prime divisors.
hammings :: [Integer]
hammings = 1 : map (2*) hammings `merge` map (3*) hammings `merge` map (5*) hammings

merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x > y = y : merge (x:xs) ys
    | otherwise = x : merge xs ys

--Generate the look-and-say sequence: [1,11,21,1211,111221,312211,13112221,1113213211,…].
lookNsay :: [Integer]
lookNsay = iterate (\x -> read $ lns $ show x) 1

lns :: String -> String
lns s
    | null s    = s
    | otherwise = lnsCall s e
    where (e:se) = s

lnsCall :: String -> Char -> String
lnsCall s c = show n ++ [c] ++ (lns $ ts)
    where (hs, ts) = span (== c) s
          n = length hs
--lnsCall s c = show n ++ [c] ++ (lns $ ts)
--    where (n, ts) = splitTakeWhile s c 0

--splitTakeWhile :: String -> Char -> Integer -> (Integer, String)
--splitTakeWhile s c n
--    | null s || (e /= c) = (n, s)
--    | otherwise = splitTakeWhile se c $ succ n
--    where (e:se) = s

--Generate the sequences of rows of the Tartaglia triangle
--(also known as Pascal’s triangle): [[1],[1,1],[1,2,1],[1,3,3,1],…].
tartaglia :: [[Integer]]
tartaglia = iterate (\x -> 1 : pascal x) [1]

pascal :: [Integer] -> [Integer]
pascal (x:[]) = [x]
pascal (x1:x2:sx) = x1 + x2 : pascal (x2:sx)

