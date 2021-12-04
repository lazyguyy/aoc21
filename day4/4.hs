import Text.Read (readMaybe)
import Data.List (transpose, minimumBy, maximumBy)

type BingoField = (Int, Bool)
type Bingo = [[BingoField]]
type BingoResult = (Int, Int)

applyMark :: Bingo -> Int -> Bingo
applyMark b x = map (map (\(v, m) -> (v, v == x || m))) b

makeBingo :: String -> Maybe Bingo
makeBingo s = (sequence $ map readMaybe $ words s) >>= toSquare . flip zip (replicate 25 False)
    where
        toSquare :: [BingoField] -> Maybe Bingo
        toSquare l
            | length l /= 25 = Nothing
            | otherwise      = Just $ splitIn 5 l

splitIn :: Int -> [a] -> [[a]]
splitIn n xs
    | length xs == 0 = []
    | length xs < n = [xs]
    | otherwise     = take n xs : (splitIn n $ drop n xs)

checkRows :: Bingo -> Bool
checkRows = or . map (and . (map snd))

checkCols :: Bingo -> Bool
checkCols = checkRows . transpose

checkBingo :: Bingo -> Bool
checkBingo b = checkCols b || checkRows b 

getScore :: Int -> Bingo -> Int
getScore n = (*n) . sum . map fst . filter (not . snd) . concat 

simulate :: Bingo -> [Int] -> BingoResult
simulate b vs = (k, score)
    where
        result = runBingo b vs
        score = uncurry (flip getScore) $ head result
        k = length vs - length result

runBingo :: Bingo -> [Int] -> [(Bingo, Int)]
runBingo b vs = dropWhile (not . checkBingo . fst) $ flip zip vs $ tail $ scanl applyMark b vs

findFirst :: [Bingo] -> [Int] -> BingoResult
findFirst bs vs =  minimumBy (\x y -> compare (fst x) (fst y)) $ map (flip simulate vs) bs

findLast :: [Bingo] -> [Int] -> BingoResult
findLast bs vs =  maximumBy (\x y -> compare (fst x) (fst y)) $ map (flip simulate vs) bs

split :: Char -> String -> [String]
split d [] = []
split d s = (takeWhile (/= d) s) : (split d $ dtail $ dropWhile (/= d) s)
    where
        dtail [] = []
        dtail l = tail l


day4a :: String -> Maybe Int
day4a input = snd <$> result
    where
        vs = sequence $ map (readMaybe) $ split ',' $ head $ lines input
        bingos = sequence $ map makeBingo $ map unlines $ splitIn 5 $ filter ((/= 0) . length) $ tail $ lines input
        result = findFirst <$> bingos <*> vs


day4b :: String -> Maybe Int
day4b input = snd <$> result
    where
        vs = sequence $ map (readMaybe) $ split ',' $ head $ lines input
        bingos = sequence $ map makeBingo $ map unlines $ splitIn 5 $ filter ((/= 0) . length) $ tail $ lines input
        result = findLast <$> bingos <*> vs

main = do
    input <- getContents
    print $ day4a input
    print $ day4b input

toString :: Bingo -> String
toString = unlines . ("---------------":) . map (singleLine . reverse) 
    where
        singleLine :: [BingoField] -> String
        singleLine b =  (foldl (\acc (v, b) -> if b then (pad 3 $ show v) ++ acc else "   "++acc) "" b)

pad :: Int -> String -> String
pad n s
    | n > length s = replicate (n - length s) ' ' ++ s
    | otherwise = s
