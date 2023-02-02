fact :: [Int]
fact = scanl (*) 1 [1..]

factN :: Int -> [Int]
factN x = take (x+1) $ fact
