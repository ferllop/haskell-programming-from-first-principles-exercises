data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. What is the type of myCar?
-- Vehicle

-- 2. Define the functions:
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3. Write a function to tell us the manuyfacturer of a piece of data:
getManufacturer :: Vehicle -> Manufacturer
getManufacturer (Car manufacturer _) = manufacturer

-- 4. What will happen if you use this on Plane data?
-- It will fail with a non-exhaustive error 

-- 5. Add the size of the plane as an argument to the Plane constructor and wherever is it appropriate:
data Size = Size Integer deriving (Eq, Show)
data Vehicle' = Car' Manufacturer Price | Plane' Airline Size deriving (Eq, Show)
doge' = Plane' PapuAir (Size 234)
isPlane' (Plane' _ _) = True
isPlane' _ = False



