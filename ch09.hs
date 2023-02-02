module Ch09 where

myHead :: [t] -> t
myHead (x : _) = x

safeMyHead :: [t] -> Maybe t
safeMyHead [] = Nothing
safeMyHead (x : _) = Just x


eftBool :: Bool -> Bool -> [Bool]
eftBool a b 
  | a == b = [a]
  | a == False = [False, True]
  | otherwise = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b = go a b []
  where go x y accum
            | x == y = x:accum
            | x > y = accum
            | otherwise = go x (pred y) (y:accum)

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
  where go a b accum = if a > b then accum else go (a+1) b (accum ++ [a]) 

eftChar :: Char -> Char -> [Char]
eftChar x y = map toChar (eftInt (fromEnum x) (fromEnum y))
                where toChar x = toEnum x :: Char

-- Exercise 1
myWords :: String -> [String]
myWords words = go words []
    where startsWithSpace ws = takeWhile (/=' ') ws == ""
          go ws accum 
                | ws == "" = accum
                | startsWithSpace ws = go (dropWhile (==' ')  ws) accum
                | not (startsWithSpace ws) = go (dropWhile (/=' ') ws) (accum ++ [(takeWhile (/=' ') ws)])
                          

-- Square cube
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
ex1 = [(x,y) | x <- mySqr, y <- myCube]
ex2 = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
ex3 = length ex2

filterEx1 xs = filter (\x -> mod x 3 == 0) xs
filterEx2 = length . filterEx1
filterEx3 sentence = filter (not . isArticle) (words sentence)
  where isArticle word = elem word ["a", "an", "the"]

zipEx1 :: [a] -> [b] -> [(a,b)]
zipEx1 as bs = go as bs []
  where go xs ys accum
            | length xs == 0 || length ys == 0 = accum
            | otherwise = go (tail xs) (tail ys) (accum ++ [(head xs, head ys)])

zipWithEx2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithEx2 f as bs = go f as bs []
  where go f xs ys accum
            | length xs == 0 || length ys == 0 = accum
            | otherwise = go f (tail xs) (tail ys) (accum ++ [f (head xs) (head ys)])

zipEx3 :: [a] -> [b] -> [(a,b)]
zipEx3 xs ys = zipWithEx2 (,) xs ys

