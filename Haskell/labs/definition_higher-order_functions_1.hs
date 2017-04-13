--only use recursion to implement myFoldl, myFoldr, myIterate, myUntil and myZip.


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ a [] = a
myFoldl f a (b:sb) = myFoldl f (f a b) sb

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (a:sa) = f a (myFoldr f b sa)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil f1 f2 a
    | f1 a      = a
    | otherwise = myUntil f1 f2 (f2 a)

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x y -> [f x] ++ y) []
--myMap f l = [f x | x <- l]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = myFoldr (\x y -> exclude f x ++ y) []
--myFilter f l = [x | x <- l, f x]

exclude :: (a -> Bool) -> a -> [a]
exclude f a
    | f a       = [a]
    | otherwise = []


myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f l = myFoldr (&&) True (myMap f l)
--myAll f l = and $ map f l

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f l = myFoldr (||) False (myMap f l)
--myAny f l = or $ map f l

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a:sa) (b:sb) = (a, b) : myZip sa sb

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 = myMap (\x -> f (fst x) (snd x)) (myZip l1 l2)
--myZipWith _ [] _ = []
--myZipWith _ _ [] = []
--myZipWith f l1 l2 = [ f x y | (x, y) <- myZip l1 l2]