import Data.Array

main = do
   input <- readFile "day2.in"
   print $ (runProgram (changeInitial (formArray input)) 0) ! 0


formList :: String -> [Int]
formList str = case break (==',') str of
                   (a, ',':b) -> (read a :: Int) : formList b
                   (a, "")    -> [(read a :: Int)]

formArray :: String -> Array Int Int
formArray str = array (0, length list - 1) (zip [0,1..(length list)] list)
   where list = formList str

changeInitial :: Array Int Int -> Array Int Int
changeInitial arr = arr//[(1, 12), (2, 2)]

runProgram :: Array Int Int -> Int -> Array Int Int
runProgram arr x = 
   case (arr ! x) of
      1 -> runProgram (arr//[(arr ! (x+3), arr ! (arr ! (x+1)) + arr ! (arr ! (x+2)))]) (x+4)
      2 -> runProgram (arr//[(arr ! (x+3), arr ! (arr ! (x+1)) * arr ! (arr ! (x+2)))]) (x+4)
      99 -> arr
