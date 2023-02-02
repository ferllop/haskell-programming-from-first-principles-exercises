import Data.Bool

mapBool = map (\x -> bool x (-x) (x==3)) [1..10]
