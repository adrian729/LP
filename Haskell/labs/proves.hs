mitjana :: [Int] -> Maybe Int
mitjana [] = Nothing
mitjana l = Just (sum l `div` (length l))