{- PROBLEM 1
It is often needed to convert a number written in arabic symbols,
to a string of its textual representation, i.e. for financial documents.
Write a function intToWords that transcribes an integer into its 
textual representation in format "digit-digit-digit-...". 
-}

intToWords :: (Num a, Show a) => a -> String
intToWords x = init (foldr step [] (show x))
    where step x ys | x == '-' = "minus-" ++ ys
                    | x == '1' = "one-" ++ ys
                    | x == '2' = "two-" ++ ys
                    | x == '3' = "three-" ++ ys
                    | x == '4' = "four-" ++ ys
                    | x == '5' = "five-" ++ ys
                    | x == '6' = "six-" ++ ys
                    | x == '7' = "seven-" ++ ys
                    | x == '8' = "eight-" ++ ys
                    | x == '9' = "nine-" ++ ys
                    | x == '0' = "zero-" ++ ys
                    |otherwise = error "not match number"

problem1 = do
    print "Problem 1"
    print $ intToWords  150  -- "one-five-zero"
    print $ intToWords    0  -- "zero"
    print $ intToWords (-10) -- "minus-one-zero"

main = do
    problem1    

