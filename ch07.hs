module Ch07 where


dodgy x y = x +y *10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
   | y >= 0.9 = 'A'
   | y >= 0.8 = 'B'
   | y >= 0.7 = 'C'
   | y >= 0.59 = 'D'
   | y < 0.59 = 'F'
   where y = x / 100

pal xs 
   | xs == reverse xs = True
   | otherwise        = False

numbers x
   | x < 0 = -1
   | x == 0 = 0
   | x > 0 = 1

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast = fst (divMod x 10)
        d = snd (divMod xLast 10)

hunsD :: Integral a => a -> a
hunsD x = d2
  where d = x `div` 100
        d2 = d `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  True -> y
  False -> x

foldBool2 _ y True = y
foldBool2 x _ False = x

g :: (a -> b) -> (a, c) -> (b, c)
g f t = (a, b)
  where a = f (fst t)
        b = snd t

g' f (a,b) = (f a, b)

