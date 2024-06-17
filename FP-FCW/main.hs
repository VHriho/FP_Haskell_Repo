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

main = do
    problem1