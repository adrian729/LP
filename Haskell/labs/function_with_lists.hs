myLength :: [Int] -> Int
myLength l = case l of
                []     -> 0
                (e:sl) -> 1 + (myLength sl)




myMaximum :: [Int] -> Int 
myMaximum (e:se)
    | se == [] = e
    | e < ms           = ms
    | otherwise        = e
    where ms = (myMaximum se)




average :: [Int] -> Float
average l
    | l == []   = fromIntegral 0
    | otherwise = (fromIntegral s) / (fromIntegral n)
    where (s, n) = sumNlength l

sumNlength :: [Int] -> (Int, Int)
sumNlength (x:xs)
    | xs == []  = (x, 1)
    | otherwise = (x + s, n + 1)
    where (s, n) = sumNlength xs




buildPalindrome :: [Int] -> [Int]
buildPalindrome l = reverseList l ++ l

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (e:se) = (reverseList se) ++ [e]




remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove l [] = l
remove (e:se) (r:sr)
    | e == r    = remove (remove se [r]) sr
    | otherwise = remove ([e] ++ remove se [r]) sr




flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (l1:l2) = l1 ++ (flatten l2)




oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([], [])
oddsNevens (n:sn)
    | m == 0    = (odds, (n:evens))
    | otherwise = ((n:odds), evens)
    where m = mod n 2
          (odds, evens) = oddsNevens sn




primeDivisors :: Int -> [Int]
primeDivisors n = primeDivisors2 n 2

primeDivisors2 :: Int -> Int -> [Int]
primeDivisors2 n d
    | n == 1              = []
    | m == 0 && isPrime d = d : (primeDivisors2 (divAll n d) (d + 1))
    | otherwise           = primeDivisors2 n (d + 1)
    where m = mod n d

divAll :: Int -> Int -> Int
divAll n d
    | m > 0     = n    
    | otherwise = divAll (div n d) d
    where m = mod n d

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = isPrimeRec n 2

isPrimeRec :: Int -> Int -> Bool
isPrimeRec n m
    | m * m > n      = True
    | (mod n m) == 0 = False
    | otherwise      = isPrimeRec n (m + 1)