import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

type Path = (Set.Set (Int, Int), Map.Map (Int, Int) Int, (Int, Int))

main = do
   input <- readFile "day3.in"
   print $ manhattanDistance $ getMinimumIntersection $ map getPaths $ map formList $ lines input

formList :: String -> [String]
formList str = case break (==',') str of
                   (a, ',':b) -> a : formList b
                   (a, "")    -> [a]

getPath :: [String] -> Path
getPath strs = foldl appendToPath (Set.empty, Map.empty, (0, 0)) strs

appendToPath :: Path -> String -> Path
appendToPath (set, map, (x, y)) str =
   case (str) of
      ('R':tl) -> (foldr Set.insert rightArr set)
      ('L':tl) -> 
      ('U':tl) ->
      ('D':tl) ->
   where rightArr = [(i, y) | i <- [x+1..(x+(read tl :: Int))]]
         leftArr = [(i, y) | i <- reverse [(x-(read tl :: Int))..x-1]]
         upArr = [(x, j) | j <- [y+1..(y+(read tl :: Int))]]
         downArr = [(x, j) | j <- reverse [(y-(read tl :: Int))..y-1]]

insertToPathMap :: Map.Map (Int, Int) Int -> ((Int, Int), Int) -> Map.Map (Int, Int) Int

getMinimumIntersection :: [[(Int, Int)]] -> (Int, Int)
getMinimumIntersection (fst:snd:tl) = head $ sortBy manhattanOrdering $ Set.toList $ Set.intersection (Set.fromList fst) (Set.fromList snd)

manhattanOrdering :: (Int, Int) -> (Int, Int) -> Ordering
manhattanOrdering x y
   | manhattanDistance x > manhattanDistance y = GT
   | manhattanDistance x < manhattanDistance y = LT
   | manhattanDistance x == manhattanDistance y = EQ

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (a,b) = (abs a) + (abs b)
