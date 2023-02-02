import Data.Char

startOf :: Int -> Int
startOf position = if isLower . chr $ position then ord 'a' else ord 'A' 

circle :: Int -> Int -> Int -> Int
circle size shift position = startOf position + mod (position + shift - (startOf position)) size

shiftChar :: Int -> Char -> Char
shiftChar shift = chr . (circle 26 shift) . ord 

caesar :: Int -> String -> String
caesar shift xs = map (shiftChar shift) xs 



