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

main = do
    problem1