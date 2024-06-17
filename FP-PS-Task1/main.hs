import Data.List

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

{- PROBLEM 2
Write a function findMaxFrequency that for a given homogenous list of type a
returns a pair (a, Int) of the most frequent element (any, if there are more than one) and its frequecy. For an empty list throw an error. 
-}

findMaxFrequency :: (Ord a) => [a] -> (a, Int)
findMaxFrequency [] = error "empty list"
findMaxFrequency lst = myTranspose (maximum (foldr step [] (group (sort lst))))
    where step x ys = (length x, head x) : ys

myTranspose :: (b, a) -> (a, b)
myTranspose (x, y) = (y, x) 

problem2 = do
    print "Problem 2"
    print $ findMaxFrequency [1,2,1,3,1,4]   -- (1, 3)
    print $ findMaxFrequency [1,1,2,2]       -- (1, 2) or (2, 2)
    print $ findMaxFrequency "some sentence" -- ('e', 4)
    -- print $ findMaxFrequency ([] :: String)  -- error

main = do
    problem1
    problem2    

