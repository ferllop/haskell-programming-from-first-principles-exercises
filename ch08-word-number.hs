module WordNumber where

import Data.List (intersperse)
import Debug.Trace (trace)

digitWord :: Int -> String
digitWord n 
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"

digits :: Int -> [Int]
digits n = go n []
  where go num accum
           | num < 10 = num:accum
           | otherwise = go rest (last:accum)
                         where last = mod num 10
                               rest = div num 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitWord . digits
