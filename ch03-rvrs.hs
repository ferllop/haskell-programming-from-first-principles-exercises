module Reverse where

rvrs :: String -> String
rvrs x = last ++ secondLast ++ first
    where 
        first = take 5 x
        secondLast = take 4 (drop 5 x)
        last = drop 9 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
