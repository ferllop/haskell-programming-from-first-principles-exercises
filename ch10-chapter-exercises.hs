-- Given the following sets of consonants and vowels:
stops = "pbtdkg"
vowels = "aeiou"

-- a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations.
makeTriplets :: String -> String -> [(Char,Char,Char)]
makeTriplets stops vowels = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

-- b) Modify that function so that it only returns the combinations that begin with a p.
makeTriplets' :: String -> String -> [(Char,Char,Char)]
makeTriplets' stops vowels = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

-- c) Now set up lists of nouns and verbs (instead of stops and vowels), and modify the function to make tuples representing possible noun-verb-noun sentences.
nouns = ["Maria", "Ana", "Silvia"]
verbs = ["walks with", "talks to", "study with"]

makeTriplets'' :: [String] -> [String] -> [(String, String, String)]
makeTriplets'' nouns verbs =  [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]
