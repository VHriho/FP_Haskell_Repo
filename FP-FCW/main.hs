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

main = do
    problem1
    problem2