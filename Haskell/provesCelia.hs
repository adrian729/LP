absValue :: Int -> Int
absValue x
    | x < 0     = -x
    | otherwise =  x

power :: Int -> Int -> Int
power 0 _ = 0
power _ 0 = 1
power x y = x * power x (y - 1)

isPrime :: Int -> Bool
isPrime 2 = True
isPrime x = isPrime' x 2

isPrime' :: Int -> Int -> Bool
isPrime' x y
    | y*y > x   = True
    | otherwise = mod x y /= 0 && isPrime' x (y+1)