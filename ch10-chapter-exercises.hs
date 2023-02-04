-- Warm-up and review
-- 1. Given the following sets of consonants and vowels:
stops = "pbtdkg"
vowels = "aeiou"

-- a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations.
makeTriplets :: String -> String -> [(Char,Char,Char)]
makeTriplets stops vowels = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

-- b) Modify that function so that it only returns the combinations that begin with a p.
makeTriplets' :: String -> String -> [(Char,Char,Char)]
makeTriplets' stops vowels = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

-- c) Now set up lists of nouns and verbs (instead of stops and vowels), and modify the function to make tuples representing possible noun-verb-noun sentences.
nouns = ["Maria", "Ana", "Silvia"]
verbs = ["walks with", "talks to", "study with"]

makeTriplets'' :: [String] -> [String] -> [(String, String, String)]
makeTriplets'' nouns verbs =  [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]


-- 2. What does the following function?
seekritFunc x =
    div (sum (map length (words x )))
        (length (words x))
-- Answer: gives the average length of the words containing a sentence

-- 3. Rewrite seekritFunc to make it use fractional division
fracAverageLenghtWords x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- Rewriting functions using folds
-- For instance:

-- direct recursion, not using &&
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs

-- direct recursion, using &&
myAnd' [] = True
myAnd' (x:xs) = x && myAnd xs

-- fold, not point-free
myAnd'' :: [Bool] -> Bool
myAnd'' = foldr (\a b -> if a == False then False else b) True

-- fold, both myAnd and the folding function are point-free
myAnd''' :: [Bool] -> Bool
myAnd''' = foldr (&&) True

-- You don't need to write al variations for each examlpe, but the more variations you write, the deeper your understanding of these functions will become.

-- 1. myOr returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = x || myOr' xs

myOr'' :: [Bool] -> Bool
myOr'' = foldr (\x start -> if x == True then True else start) False

myOr''' :: [Bool] -> Bool
myOr''' = foldr (||) False

-- 2. myOr returns True if any Bool in the list is True
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if (f x) == True then True else myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' f (x:xs) = f x || myAny' f xs

myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f = foldr (\x start -> if (f x) == True then True else start) False

myAny''' :: (a -> Bool) -> [a] -> Bool
myAny''' f = foldr (\x start -> start || f x) False

-- 3. Write two versions of myElem. One should use folding and the other should use any:
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y _ -> x == y) False
myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (==x)

-- 4. Implement myReverse. Dont' worry trying to make it lazy:
myReverse :: [a] -> [a]
myReverse = foldr (flip (++) . (:[])) []

-- 5. Write myMap in terms of foldr
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x start -> (f x):start) []

-- 6. Write myFilter in terms of foldr
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x start -> if f x then (x:start) else start) [] 

-- 7. squish flattens a list of lists into a list.
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8. squishMap maps a function over a list and concatenates the result.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f

-- 9. squishAgain flattens a list of lists into a list. Reuse squishMap.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returns GT for:
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\start x -> if f start x == LT then x else start) x xs

-- 11. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returns LT for:
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\start x -> if f start x == GT then x else start) x xs
