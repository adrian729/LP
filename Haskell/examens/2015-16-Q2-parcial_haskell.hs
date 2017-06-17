-- Problema 1
allsets :: a -> [[a]]
allsets e = iterate ([e] ++) [] --foldr ((++) . (\x -> [take x $ repeat e])) [] [0,1..]

-- Problema 2
alldivisors :: Int -> [[Int]]
alldivisors n =
    map (\x -> take (timesDiv n x) $ repeat x) $ filter (\x -> mod n x == 0) [2,3..n]

timesDiv :: Int -> Int -> Int
timesDiv x d
    | mod x d == 0 = succ $ timesDiv (div x d) d
    | otherwise    = 0

-- Problema 3.1
data Expr a =
    Var String
    | Const a
    | Func String [Expr a]
    deriving (Show, Eq)

-- Problema 3.2
constLeafs :: Expr a -> [a]
constLeafs (Const x)  = [x]
constLeafs (Func _ lexprs) = foldr ((++) . constLeafs) [] lexprs 
constLeafs _ = []

-- Problema 3.3
instance Functor Expr where
    fmap g (Var x) = Var x
    fmap g (Const x) = Const $ g x
    fmap g (Func s lexprs) = Func s $ map (fmap g) lexprs

-- Problema 4
join :: Eq a => [(String, a)] -> [(String, a)] -> Maybe [(String, a)]
join l [] = Just l
join [] l = Just l
join l1@(e1:se1) l2@(e2:se2)
    | str1 == str2 && (val1 /= val2)
                   = Nothing
    | str1 == str2 = joinToMaybe e1 $ join se1 se2
    | str1 < str2  = joinToMaybe e1 $ join se1 l2
    | otherwise    = joinToMaybe e2 $ join l1 se2
    where (str1, val1) = e1
          (str2, val2) = e2

joinToMaybe :: (String, a) -> Maybe [(String, a)] -> Maybe [(String, a)]
joinToMaybe _ Nothing = Nothing
joinToMaybe e (Just l) = Just (e:l)

-- Problema 5
match :: Eq a => Expr a -> Expr a -> Maybe [(String, (Expr a))]
match (Var varName) expr = Just [(varName, expr)]
match (Func name1 l1) (Func name2 l2)
    | name1 /= name2 = Nothing
    | otherwise      =
          foldr (join') (Just []) $ map (\x -> match (fst x) (snd x)) $ zip l1 l2
match expr1 expr2
    | expr1 == expr2 = Just []
    | otherwise      = Nothing

join' :: Eq a => Maybe [(String, a)] -> Maybe [(String, a)] -> Maybe [(String, a)]
join' Nothing _ = Nothing
join' _ Nothing = Nothing
join' (Just l1) (Just l2) = join l1 l2