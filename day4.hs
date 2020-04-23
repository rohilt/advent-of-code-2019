import System.IO
import Data.Char

main = do
   print $ length $ filter validPassword [240298..784956]

validPassword :: Int -> Bool
validPassword x = (twoConsecutiveNew $ convertToDigits x) && (nonDecreasing $ convertToDigits x)

convertToDigits :: Int -> [Int]
convertToDigits i = map digitToInt $ show i

twoConsecutive :: [Int] -> Bool
twoConsecutive (a:b:tl) = (a == b) || twoConsecutive (b:tl)
twoConsecutive _ = False

twoConsecutiveNew :: [Int] -> Bool
twoConsecutiveNew x = or [(length $ filter (==i) x) == 2 | i <- [0..9]]

nonDecreasing :: [Int] -> Bool
nonDecreasing (a:b:tl) = (a <= b) && nonDecreasing (b:tl)
nonDecreasing _ = True
