module PoemLines where

mySplit :: Char -> String -> [String]
mySplit separator str = go str []
    where startsWith ch ws = takeWhile (/= ch) ws == ""
          go ws accum 
                | ws == "" = accum
                | startsWith separator ws = go (dropWhile (== separator) ws) accum
                | not (startsWith separator ws) = go (dropWhile (/= separator) ws) (accum ++ [(takeWhile (/= separator) ws)])

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"

sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = mySplit '\n'

shouldEqual =  
  [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal? "
  ++ show (myLines sentences
           == shouldEqual)

myWords :: String -> [String]
myWords = mySplit ' '
