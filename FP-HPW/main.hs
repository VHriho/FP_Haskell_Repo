import Data.Fixed

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

-- Boolean Operators
setIntersection :: IntSet -> IntSet -> IntSet
setIntersection a b x = a x && b x 

setUnion :: IntSet -> IntSet -> IntSet
setUnion a b x = a x || b x

setComplement :: IntSet -> IntSet -> IntSet
setComplement a b x = a x && not (b x)

-- Set generation
addToSet :: Int -> IntSet -> IntSet
addToSet value set x = value == x || set x

deleteFromSet :: Int -> IntSet -> IntSet
deleteFromSet value set x = set x && value /= x

equalIntSet :: IntSet -> IntSet -> Int -> Bool
equalIntSet a b x = a x == b x

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
    print "Part D."
    print "Boolean Operators: "
    print $ "setIntersection [0;2] [1;3] 1: " ++ show(setIntersection (interval 0 2) (interval 1 3) 1)
    print $ "setIntersection [0;2] [4;5] 3: " ++ show(setIntersection (interval 0 2) (interval 4 5) 3)
    print $ "setUnion [0;3] [3;4] 2: " ++ show(setUnion (interval 0 3) (interval 3 4) 2)
    print $ "setUnion [0;2] [4;5] 3: " ++ show(setUnion (interval 0 2) (interval 4 5) 3)
    print $ "setComplement [0;1] [0;5] 2: " ++ show(setComplement (interval 0 1) (interval 0 5) 2)
    print $ "setComplement allInts [0;1] 2: " ++ show(setComplement allInts (interval 0 1) 2)
    print "Set generation: "
    print $ "addToSet 4 [0;3]: " ++ show(addToSet 4 (interval 0 3) 4)
    print $ "deleteFromSet 4 [0;4]: " ++ show(deleteFromSet 4 (interval 0 4) 4)
    putStrLn ""
    print "Part E."
    print $ "equalIntSet allInts allInts: " ++ show(equalIntSet allInts allInts 1)
    print $ "equalIntSet emptySet allInts: " ++ show(equalIntSet emptySet allInts 1)
    putStrLn ""

-- Problem 3.

type Stack = String

isOperator :: String -> Bool
isOperator x = elem x ["+", "-", "*", "/", "%", "^"]

isLeftAssociative :: String -> Bool
isLeftAssociative x = elem x ["+", "-", "*", "/", "%"]

isLeftParenthesis :: String -> Bool
isLeftParenthesis x = x == "("

isRightParenthesis :: String -> Bool
isRightParenthesis x = x == ")"

priority :: String -> Int
priority x  | x == "+" = 1
            | x == "-" = 1
            | x == "*" = 2
            | x == "/" = 2
            | x == "^" = 3
            | x == "%" = 4 

parse :: String -> Stack
parse expr = helpParse (words expr) [] [] 
    where
        helpParse [] [] output = unwords (reverse output)
        helpParse [] stackOfOperators output
            | isLeftParenthesis (head stackOfOperators) = error "Incorrect brackets"
            | otherwise = helpParse [] (tail stackOfOperators) (head stackOfOperators : output)
        helpParse input stackOfOperators output
            | isLeftParenthesis  (head input) = helpParse (tail input) (head input : stackOfOperators) output
            | isRightParenthesis (head input) && null stackOfOperators = error "Incorrect brackets"
            | isRightParenthesis (head input) && isLeftParenthesis (head stackOfOperators) = helpParse (tail input) (tail stackOfOperators) output
            | isRightParenthesis (head input) = helpParse input (tail stackOfOperators) (head stackOfOperators : output)
            | isOperator (head input) && null stackOfOperators = helpParse (tail input) (head input : stackOfOperators) output
            | isOperator (head input) = if not (isLeftParenthesis (head stackOfOperators)) && (priority (head stackOfOperators) >= priority (head input) && isLeftAssociative (head input))
                then helpParse input (tail stackOfOperators) (head stackOfOperators : output)
                else helpParse (tail input) (head input : stackOfOperators) output
            | otherwise = helpParse (tail input) stackOfOperators (head input : output)

eval expr = head (foldl step [] (words expr))
    where   
        step (x:y:ys) "*" = (x * y)  :ys
        step (x:y:ys) "+" = (x + y)  :ys
        step (x:y:ys) "-" = (y - x)  :ys
        step (x:y:ys) "/" = (y / x)  :ys
        step (x:y:ys) "^" = (y ** x) :ys
        step (x:y:ys) "%" = mod' x y : ys
        step xs number = read number:xs

problem3 = do 
    print "Problem 3."
    print "Part A."
    print $ "Parse 10 * 2 ^ ( 3 - 1 ) * 3.5 to RPN: " ++ parse "10 * 2 ^ ( 3 - 1 ) * 3.5" -- 10 2 3 1 - ^ * 3.5 *
    print $ "Parse ( 10 / ( 2 % 2 ) ) + 1 to RPN: " ++ parse "( 10 / ( 2 % 2 ) ) + 1" -- 10 2 2 % / 1 +
    print $ "Parse ( 2 + 2 ) + ( 3 ^ 2 % 2 ) to RPN: " ++ parse "( 2 + 2 ) + ( 3 ^ 2 % 2 )" -- 2 2 + 3 2 2 % ^ +
    putStrLn ""
    print "Part B."
    print $ "Eval RPN: " ++ parse "10 * 2 ^ ( 3 - 1 ) * 3.5" ++ "  = " ++ show(eval (parse "10 * 2 ^ ( 3 - 1 ) * 3.5"))
    print $ "Eval RPN: " ++ parse "( 10 / ( 2 % 2 ) ) + 1" ++ "        = " ++ show (eval (parse "( 10 / ( 2 % 2 ) ) + 1"))
    print $ "Eval RPN: " ++ parse "( 2 + 2 ) + ( 3 ^ 2 % 2 )" ++ "     = " ++ show(eval (parse "( 2 + 2 ) + ( 3 ^ 2 % 2 )"))

main = do
    problem1
    problem2
    problem3