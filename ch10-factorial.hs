fact :: [Integer]
fact = 1 : scanl (\x accum -> (x + 1) * accum) 1 fact

factN x = take (x+1) $ fact
