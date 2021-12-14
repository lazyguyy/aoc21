gauss :: Int -> Int
gauss n = n * (n + 1) `div` 2

split :: Char -> String -> [String]
split _ [] = []
split c s = (takeWhile (/=c) s) : (split c $ safetail $ dropWhile(/=c) s)
    where
        safetail [] = []
        safetail (x:xs) = xs

day7a :: String -> Int
day7a input = minimum $ map (\x -> sum $ map (abs . (x -)) poss) poss
    where
        poss = map read $ split ',' input


day7b :: String -> Int
day7b input = minimum $ map (\x -> sum $ map (gauss . abs . (x -)) poss) poss
    where
        poss = map read $ split ',' input


main = do
    input <- getContents
    print $ day7a input
    print $ day7b input
