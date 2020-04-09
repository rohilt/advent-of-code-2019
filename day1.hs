import System.IO

main = do
   input <- readFile "day1.in"
   print $ getTotalFuelNeeded $ lines input

getTotalFuelNeeded :: [String] -> Int
getTotalFuelNeeded [] = 0
getTotalFuelNeeded (str:tl) = (getFuelNeeded (read str :: Int)) + getTotalFuelNeeded tl

getFuelNeeded :: Int -> Int
getFuelNeeded x
   | result >= 0 = result + getFuelNeeded result
   | otherwise = 0
   where result = (x `div` 3) - 2
