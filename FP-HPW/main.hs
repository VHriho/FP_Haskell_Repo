-- Problem 1.

diff f dx x = (f (x+dx) - f x) / dx

newton_iter f f' x k
    |k <= 0    = x 
    |otherwise = newton_iter f f' (x - f x / f' x) (k-1) 

problem1 = do
    let x = 0.5
    print "Problem 1"
    print "For sin(x), x0 = 0.5, dx = 0.01" 
    print $ "Result: " ++ show (newton_iter sin (diff sin 0.01) x 100)
    print "For x^3-328x^2-1999x-1670, x0 = 100, dx = 0.01" 
    print $ "Result: " ++ show (newton_iter (\x -> x**3 - 328*x**2 - 1999*x - 1670) (diff (\ x -> x ** 3 - 328 * x ** 2 - 1999 * x - 1670) 0.01) 100 100)
    putStrLn ""


-- Problem 2.

type IntSet = (Int -> Bool)

isMember :: IntSet -> Int -> Bool
isMember f x = f x

emptySet :: IntSet
emptySet x = False
allInts :: IntSet
allInts x = True

-- interval x y contains all the integers in [x;y]
interval :: Int -> Int -> IntSet
interval lBound uBound x
    |lBound < uBound  = (lBound <= x) && (x <= uBound)
    |lBound == uBound = lBound == x
    |otherwise        = (uBound <= x) && (x <= lBound)

coIntegers :: Int -> IntSet
coIntegers k b = helpСoIntegers k b == 1 
    where
        helpСoIntegers k 0 = k
        helpСoIntegers 0 b = b
        helpСoIntegers k b  | k > b     = helpСoIntegers (k-b) b
                            | otherwise = helpСoIntegers k (b-k)

problem2 = do 
    print "Problem 2"
    print "Part A."
    print $ "emptySet x: " ++ show(emptySet 0)
    print $ "allInts x: " ++ show(allInts 1)
    putStrLn ""
    print "Part B."
    print "interval lBound uBound x: " 
    print $ "[0;1] 2: " ++ show(interval 0 1 2)
    print $ "[0;2] 1: " ++ show(interval 0 2 1)
    putStrLn ""
    print "Part C."
    print "Coprime integers: "
    print $ "Coprime 2 1: " ++ show(coIntegers 2 1)
    print $ "Coprime 4 2: " ++ show(coIntegers 4 2)
    putStrLn ""

main = do
    problem1
    problem2