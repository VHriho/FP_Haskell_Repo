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

{- PROBLEM 3
For a given system of types that represent a file system structure
write a function search that given a name returns a list of all paths
that correspond to that name.
-}

type Name   = String
type Path   = String
data FSNode = File Name | Dir Name [FSNode]

search :: Name -> FSNode -> [Path]
search name root = searching name root [] 
    where 
        searching name (File fileName) path 
            | fileName == name = [path ++ fileName]
            | otherwise = []
        searching name (Dir dirName (x:xs)) path 
            | dirName == name = [path ++ dirName ++ "/"] ++ searching name x (path ++ dirName ++ "/") ++ searching name (Dir dirName xs) path
            | otherwise = searching name x (path ++ dirName ++ "/") ++ searching name (Dir dirName xs) path
        searching name (Dir _ []) path = []

root = Dir "/"
  [
    Dir "folder1" 
    [
      File "file1",
      Dir  "folder2" 
      [
        File "file2",
        File "file3"
      ],
      Dir  "folder3" 
      [
        File "file3",
        File "file4"
      ],
      File "file5"
    ]
  ]

problem3 = do
    print "Problem 3"
    print $ search "file1" root -- ["//folder1/file1"]
    print $ search "file3" root -- ["//folder1/folder2/file3", "//folder1/folder3/file3"]
    print $ search "file4" root -- ["//folder1/folder3/file4"]
    print $ search "file6" root -- []

main = do
    problem1
    problem2
    problem3    

