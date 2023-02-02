import Data.Char

ex2 :: String -> String
ex2 = filter isUpper

ex3 :: String -> String
ex3 (x:xs) = toUpper x : xs

ex4 sentence = go sentence ""
    where go sentence accum
            | sentence == "" = accum
            | otherwise = go (tail sentence) (accum ++ [toUpper . head $ sentence])

ex5 :: String -> Char
ex5 sentence = toUpper (head sentence)
ex6 sentence = toUpper . head $ sentence
ex6b = toUpper . head

