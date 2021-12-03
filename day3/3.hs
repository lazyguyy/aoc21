type BinaryNumber = [Int]

toDecimal :: BinaryNumber -> Integer
toDecimal [] = 0
toDecimal (1:xs) = 2 ^ (length xs) + toDecimal xs
toDecimal (0:xs) = toDecimal xs

toBinary :: [Char] -> Maybe BinaryNumber
toBinary [] = Just []
toBinary (x:xs) = (:) <$> h <*> (toBinary xs)
    where
        h = case x of '0' -> Just 0
                      '1' -> Just 1
                      otherwise -> Nothing


day3a :: String -> Maybe Integer
day3a input = (*) <$> (toDecimal <$> gamma) <*> (toDecimal <$> epsilon)
    where
        inputs = sequence $ map toBinary $ lines input
        n = length $ lines input
        k = length $ head $ lines input
        gamma = map (\x -> if (2*x > n) then 1 else 0) . foldl (zipWith (+)) (replicate k 0) <$> inputs
        epsilon = map (1-) <$> gamma

day3b :: String -> Maybe Integer
day3b input = (*) <$> ((toDecimal . (getRating True [])) <$> inputs) <*> ((toDecimal . (getRating False [])) <$> inputs)
    where
        inputs = sequence $ map toBinary $ lines input

getRating :: Bool -> BinaryNumber -> [BinaryNumber] -> BinaryNumber
getRating _ xs [ys]  = (reverse xs) ++ ys
getRating most xs ns = getRating most (keep : xs) $ map tail $ filter ((== keep). head) ns
    where
        count = sum $ map head ns
        n = length ns
        keep = case compare (2 * count) n of EQ -> if most then 1 else 0
                                             GT -> if most then 1 else 0
                                             LT -> if most then 0 else 1

main = do
    input <- getContents
    print $ day3a input
    print $ day3b input