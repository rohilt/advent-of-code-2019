import Data.Array

main = do
   input <- readFile "day2.in"
   -- print $ (runProgram (changeInitial (formArray input) 12 2) 0) ! 0
   print $ filterOutput $ possibleInputs $ formArray input

formList :: String -> [Int]
formList str = case break (==',') str of
                   (a, ',':b) -> (read a :: Int) : formList b
                   (a, "")    -> [(read a :: Int)]

formArray :: String -> Array Int Int
formArray str = array (0, length list - 1) (zip [0,1..(length list)] list)
   where list = formList str

changeInitial :: Array Int Int -> Int -> Int -> Array Int Int
changeInitial arr x y = arr//[(1, x), (2, y)]

runProgram :: Array Int Int -> Int -> Array Int Int
runProgram arr x = 
   case (arr ! x) of
      1 -> runProgram (arr//[(arr ! (x+3), arr ! (arr ! (x+1)) + arr ! (arr ! (x+2)))]) (x+4)
      2 -> runProgram (arr//[(arr ! (x+3), arr ! (arr ! (x+1)) * arr ! (arr ! (x+2)))]) (x+4)
      99 -> arr

possibleInputs :: Array Int Int -> [(Int, Int, Int)]
possibleInputs arr = [(x, y, (runProgram (changeInitial arr x y) 0) ! 0) | x <- [0..99], y <- [0..99]]

filterOutput :: [(Int, Int, Int)] -> Int
filterOutput ((x, y, 19690720):_) = 100*x + y
filterOutput ((_, _, _):tl) = filterOutput tl
