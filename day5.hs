main = do
   input <- readFile "day5.in"
   print $ manhattanDistance $ getMinimumIntersection $ map getPaths $ map formList $ lines input

interpretIntcode :: [Int] -> [Int] -> [Int]
interpretIntcode _ [] = []
interpretIntcode input (intcode:tl)
   | intcode
