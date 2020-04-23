import System.IO
import Data.Char

main = do
   print $ length $ filter validPassword [240298..784956]

validPassword :: Int -> Bool
validPassword x = (twoConsecutive $ convertToDigits x) && (nonDecreasing $ convertToDigits x)

convertToDigits :: Int -> [Int]
convertToDigits i = map digitToInt $ show i

twoConsecutive :: [Int] -> Bool
twoConsecutive (a:b:tl) = (a == b) || twoConsecutive (b:tl)
twoConsecutive _ = False

nonDecreasing :: [Int] -> Bool
nonDecreasing (a:b:tl) = (a <= b) && nonDecreasing (b:tl)
nonDecreasing _ = True
