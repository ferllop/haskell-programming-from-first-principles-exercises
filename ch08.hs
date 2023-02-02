module Ch08 where

sumAll :: (Eq a, Num a) => a -> a
sumAll 1 = 1
sumAll x = x + sumAll (x - 1)

timesSum :: (Integral a) => a -> a -> a
timesSum _ 0 = 0
timesSum x y = x + timesSum x (y - 1)


data DividedResult = Result Integer | DividedByZero deriving (Show)

dividedBy num denom = go (abs num) (abs denom) 0 (multiplier num denom)
                        where go n d count mult
                                  | d == 0 = (DividedByZero, n)
                                  | n < d = (Result (count * mult), n)
                                  | otherwise = go (n - d) d (count + 1) mult 

                              multiplier num denom
                                  | (num < 0 && denom > 0) || (num > 0 && denom < 0) = (-1)
                                  | otherwise = 1

mc91 n = go n
           where go num
                   | num > 100 = num - 10
                   | num <= 100 = mc91 (mc91(num + 11))
