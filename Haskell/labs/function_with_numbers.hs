absValue :: Int -> Int
absValue n 
    | n < 0     = -n
    | otherwise =  n

power :: Int -> Int -> Int
power n m
    | m == 0    = 1
    | otherwise = n * (power n (m - 1))

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = isPrimeRec n 2

isPrimeRec :: Int -> Int -> Bool
isPrimeRec n m
    | m * m > n      = True
    | (mod n m) == 0 = False
    | otherwise      = isPrimeRec n (m+1)

slowFib :: Int -> Int
slowFib n
    | n < 2     = n
    | otherwise = slowFib (n-1) + slowFib (n-2)

quickFib :: Integer -> Integer
quickFib = fst . fib2
fib2 :: Integer -> (Integer, Integer)
fib2 n
    | n == 0    = (0,1)
    | otherwise = (f2, (f1+f2))
        where (f1, f2) = fib2 (n-1)