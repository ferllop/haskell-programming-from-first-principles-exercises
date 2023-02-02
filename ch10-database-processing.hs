import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
    DbDate (UTCTime (fromGregorian 1922 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbNumber 1000,
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map (\(DbDate utc) -> utc) . (filter isDbDate)
    where isDbDate (DbDate _) = True
          isDbDate _ = False

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\x accum -> if isDbNumber x then ((getNum x):accum) else accum) []
    where isDbNumber (DbNumber _) = True
          isDbNumber _ = False
          getNum (DbNumber x) = x

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr max (UTCTime (fromGregorian 0 0 0) 0) . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db)/fromIntegral (length . filterDbNumber $ db)
