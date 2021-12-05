type BinaryNumber = [Int]

toDecimal :: BinaryNumber -> Int
toDecimal = foldl (\a x -> 2 * a + x) 0

toBinary :: [Char] -> Maybe BinaryNumber
toBinary = foldr (\x acc -> (:) <$> (parseBit x) <*> acc) (Just [])
    where
        parseBit '0' = Just 0
        parseBit '1' = Just 1
        parseBit  _  = Nothing


day3a :: String -> Maybe Int
day3a input = (*) <$> (toDecimal <$> gamma) <*> (toDecimal <$> epsilon)
    where
        inputs = sequence $ map toBinary $ lines input
        n = length $ lines input
        k = length $ head $ lines input
        gamma = map (\x -> if (2 * x > n) then 1 else 0) . foldl (zipWith (+)) (replicate k 0) <$> inputs
        epsilon = map (1-) <$> gamma

day3b :: String -> Maybe Int
day3b input = (*) <$> ((toDecimal . (getRating True [])) <$> inputs) <*> ((toDecimal . (getRating False [])) <$> inputs)
    where
        inputs = sequence $ map toBinary $ lines input

getRating :: Bool -> BinaryNumber -> [BinaryNumber] -> BinaryNumber
getRating _ xs [ys]  = (reverse xs) ++ ys
getRating most xs ns = getRating most (keep : xs) $ map tail $ filter ((== keep) . head) ns
    where
        count = sum $ map head ns
        n = length ns
        keep = if ((2 * count) >= n) == most then 1 else 0

main = do
    input <- getContents
    print $ day3a input
    print $ day3b input