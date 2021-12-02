import qualified Data.Char as Char
import Text.Read (readMaybe)


data ActionType = Forward | Up | Down deriving (Read, Show)
data Action = Action ActionType Integer deriving (Show)

parseAction :: String -> Maybe Action
parseAction line = case (words line) of [a, v] -> Action <$> (readMaybe $ capitalized a) <*> (readMaybe v)
                                        otherwise -> Nothing
    where
        capitalized (h:t) = Char.toUpper h : t

day2a :: String -> Maybe (Integer, Integer)
day2a input = (foldl (flip perform) (0, 0)) <$> (sequence $ map parseAction $ lines input)
    where
        perform (Action Forward v) (x, y) = (x + v, y)
        perform (Action Up v) (x, y) = (x, y - v)
        perform (Action Down v) (x, y) = (x, y + v)

day2b :: String -> Maybe (Integer, Integer, Integer)
day2b input = (foldl (flip perform) (0, 0, 0)) <$> (sequence $ map parseAction $ lines input)
    where
        perform (Action Forward v) (x, y, a) = (x + v, y + v*a, a)
        perform (Action Up v) (x, y, a) = (x, y, a - v)
        perform (Action Down v) (x, y, a) = (x, y, a + v)

main = do
    input <- getContents
    print $ (\(x, y) -> x*y) <$> (day2a input)
    print $ (\(x, y, _) -> x*y) <$> (day2b input)
