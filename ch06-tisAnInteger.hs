module TisAnInteger where

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y


main :: IO ()
main = do
  x == y
  where
    x = TisAn 3
    y = TisAn 3
