import Text.Read (readMaybe)

countDecreases :: [Int] -> Int
countDecreases hs = length $ (filter (<0)) $ zipWith (-) hs $ tail hs


day1a :: String -> Maybe Int
day1a input = countDecreases <$> (sequence $ map (readMaybe) $ lines input)

day1b :: String -> Maybe Int
day1b input = countDecreases <$> add3 <$> (sequence $ map (readMaybe) $ lines input)

add3 :: [Int] -> [Int]
add3 xs = zipWith (+) xs $ zipWith (+) (tail xs) (tail $ tail xs)

main = do
    input <- getContents
    print $ day1a input
    print $ day1b input