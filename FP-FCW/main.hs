-- Problem 1.
int2bin ::  Int -> String
int2bin x   
    | x == 0    = "0"
    | otherwise = reverse (helpInt2bin x)

helpInt2bin :: Int -> String
helpInt2bin x 
    | x == 0    = ""
    | otherwise = show (mod x 2) ++ helpInt2bin (div x 2)

problem1 = do
    print "Problem 1"
    print $ int2bin 6


-- Problme 2.
data HList a = Atom a | List [HList a] deriving (Show)

splitInMidle :: HList a -> (HList a, HList a)
splitInMidle hlist = pair (concat $ format a) ( concat $ format b) 
    where 
        dimension = length $ listOfHLists hlist
        whereToSplit = dimension - (mod dimension 2 + div dimension 2)  
        a = take whereToSplit (listOfHLists hlist)
        b = drop whereToSplit (listOfHLists hlist)  

pair :: [HList a1] -> [HList a2] -> (HList a1, HList a2)
pair a b = (List a, List b)

listOfHLists :: HList a -> [HList a]
listOfHLists (List(x:xs)) = List [x] : help (List xs) 
    where 
        help (List []) = []
        help (List xs) = listOfHLists (List xs)

format :: [HList a] -> [[HList a]]
format (List(x:xs):ys) = [x] : helpFormat ys 
    where
        helpFormat [] = []
        helpFormat ys = format ys

problem2 = do
    print "Problem 2"
    let hlist = List [Atom 1, List [Atom 2, Atom 3], Atom 4]
    print $ splitInMidle hlist


-- Problem 3.
wordify :: String -> String 
wordify xs = foldr step [] xs where 
    step xs ys  | xs == '1' = "one" ++ " " ++ ys
                | xs == ' ' && head ys == ' ' = ys
                | xs == ' ' && head ys /= ' ' = " " ++ ys
                | xs == '2' = "two " ++ ys
                | xs == '3' = "three" ++ ys
                | xs == '4' = "four "  ++ ys
                | xs == '5' = "five " ++ ys
                | xs == '6' = "six " ++ ys
                | xs == '7' = "seven " ++ ys
                | xs == '8' = "eight " ++ ys
                | xs == '9' = "nine " ++ ys
                | xs == '0' = "zero " ++ ys
                | otherwise = xs : wordify ys

problem3 = do
    print "Problem 3"
    print $ wordify "There is 10 words"

-- Problem 4.
data Figure = Circle (Float, Float) Float | Square (Float, Float) Float | Triangle (Float, Float) (Float, Float) (Float, Float) 

class Area a where
    area :: a -> Float

instance Area Figure where 
    area (Circle _ r) = pi * r ** 2
    area (Square _ side) = side ** 2
    area (Triangle (x0, y0) (x1, y1) (x2, y2)) = 0.5 * abs ((x0-x2)*(y1-y2) - (x1-x2)*(y0-y2))

problem4 = do
    print "Problem 4"
    print $ area (Circle (0, 0) 10)
    print $ area (Square (0, 0) 10)
    print $ area (Triangle (0, 0) (5, 5) (0, 10))

main = do
    problem1
    problem2
    problem3
    problem4