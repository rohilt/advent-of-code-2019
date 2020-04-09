import Data.List

main = do
   input <- readFile "day3.in"
   print $ manhattanDistance $ getMinimumIntersection $ map getPaths $ map formList $ lines input

formList :: String -> [String]
formList str = case break (==',') str of
                   (a, ',':b) -> a : formList b
                   (a, "")    -> [a]

getPaths :: [String] -> [(Int, Int)]
getPaths strs = foldl getPath [] strs

getPath :: [(Int, Int)] -> String -> [(Int, Int)]
getPath list str =
   let (x, y) = endOfPath list in
   case (str) of
      ('R':tl) -> list ++ [(i, y) | i <- [x+1..(x+(read tl :: Int))]]
      ('L':tl) -> list ++ [(i, y) | i <- [x-1..(x-(read tl :: Int))]]
      ('U':tl) -> list ++ [(x, j) | j <- [y+1..(y+(read tl :: Int))]]
      ('D':tl) -> list ++ [(x, j) | j <- [y-1..(y-(read tl :: Int))]]

endOfPath :: [(Int, Int)] -> (Int, Int)
endOfPath [] = (0, 0)
endOfPath list = last list

getMinimumIntersection :: [[(Int, Int)]] -> (Int, Int)
getMinimumIntersection (fst:snd:tl) = head $ sortBy manhattanOrdering $ intersect fst snd

manhattanOrdering :: (Int, Int) -> (Int, Int) -> Ordering
manhattanOrdering x y
   | manhattanDistance x > manhattanDistance y = GT
   | manhattanDistance x < manhattanDistance y = LT
   | manhattanDistance x == manhattanDistance y = EQ

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (a,b) = (abs a) + (abs b)
