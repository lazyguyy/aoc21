import qualified Data.Map as Map
import Data.Map.Strict (insertWith)
import Text.Read (readMaybe)

type Coordinate = (Int, Int)
type VentMap = Map.Map Coordinate Int

addVent :: VentMap -> Coordinate -> VentMap
addVent m c = insertWith (+) c 1 m

range :: Int -> Int -> [Int]
range a b
    | a <= b    = [a..b]
    | otherwise = reverse [b..a]

isAxis :: Coordinate -> Coordinate -> Bool
isAxis f t = (fst f - fst t) * (snd f - snd t) == 0

line :: Coordinate -> Coordinate -> [Coordinate]
line f t
    | isAxis f t = (,) <$> (range (fst f) (fst t)) <*> (range (snd f) (snd t))
    | otherwise  = zip (range (fst f) (fst t)) (range (snd f) (snd t))

split :: Char -> String -> [String]
split d [] = []
split d s = (takeWhile (/= d) s) : (split d $ dtail $ dropWhile (/= d) s)
    where
        dtail [] = []
        dtail l = tail l

combineTwo :: Maybe a -> Maybe a -> Maybe (a, a)
combineTwo x y = (,) <$> x <*> y

parseTuple :: String -> Maybe Coordinate
parseTuple s = combineTwo x y
    where
        [a, b] = split ',' s
        x = readMaybe a
        y = readMaybe b

parseLine :: String -> Maybe (Coordinate, Coordinate)
parseLine s = combineTwo f t
    where
        inputs = words s
        f = parseTuple $ head inputs
        t = parseTuple $ last inputs

day5a :: String -> Maybe Int
day5a s = parsed >>= \p -> pure . length $ Map.filter (> 1) $ foldl addVent Map.empty $ concat $ map (uncurry line) $ filter (uncurry isAxis) p
    where
        parsed = sequence $ map parseLine $ lines s

day5b :: String -> Maybe Int
day5b s = parsed >>= \p -> pure . length $ Map.filter (> 1) $ foldl addVent Map.empty $ concat $ map (uncurry line) p
    where
        parsed = sequence $ map parseLine $ lines s

main = do
    input <- getContents
    print $ day5a input
    print $ day5b input