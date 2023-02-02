fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 fibs

fibsLT100 = [x | x <- fibs, x < 100]
