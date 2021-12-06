import Text.Read (readMaybe)
import System.IO

nextCount :: [Int] -> Int
nextCount xs = xs !! 6 + xs !! 8

simulate :: [Int] -> [Int] -> [Int]
simulate bs [] = bs
simulate bs (x:xs) = simulate ((age x) : makeBaby x bs) xs
    where
        makeBaby 0 bs = 8:bs
        makeBaby _ bs = bs
        age 0 = 6
        age x = x - 1

applyN :: (a -> a) -> Int -> a -> a
applyN f n = foldr (.) id $ replicate n f

numberOfFish :: [Int] -> Int -> Int
numberOfFish fish n = (length fish +) $ sum $ applyN (\xs -> (nextCount xs) : xs) (n - 9) counts
     where
        simulated = reverse $ map length $ take 10 $ iterate (simulate []) fish
        counts = zipWith (-) simulated $ tail simulated

split :: Char -> String -> [String]
split d [] = []
split d s = (takeWhile (/= d) s) : (split d $ dtail $ dropWhile (/= d) s)
    where
        dtail [] = []
        dtail l = tail l

parseFish :: String -> Maybe [Int]
parseFish = sequence . map readMaybe . split ','

main = do
    input <- getContents
    let [fish, n] = lines input
    print $ numberOfFish <$> (parseFish fish) <*> (readMaybe n)