module DigitSum where 

import Data.Char (digitToInt)

-- Evaluator for summation 
-- "1+2"   --> 3 
-- "2+4+8" --> 14 
evaluate :: String -> Int 
evaluate input = 
  sum $ map digitToInt $ splitOn '+' input 

-- splitOn :: Char -> [Char] -> [Char] == String 

splitOn :: Eq a => a -> [a] -> [a]
splitOn x xs = filter (/= x) xs  