import qualified Data.Map as M
import Data.Maybe

main = do
    input <- readFile "day6.in"
    print $ sum $ map (countOrbits $ parseInput input) (M.keys $ parseInput input)

parseInput :: String -> M.Map String String
parseInput s = foldl appendToMap M.empty (lines s)

appendToMap :: M.Map String String -> String -> M.Map String String
appendToMap map xs = M.insert (drop 4 xs) (take 3 xs) map

countOrbits :: M.Map String String -> String -> Int
countOrbits _ "COM" = 0
countOrbits m x = (countOrbits m (fromJust $ M.lookup x m)) + 1
