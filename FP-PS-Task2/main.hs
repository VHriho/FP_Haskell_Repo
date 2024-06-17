{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

{-
Dictionaries are one of the most common data structure.
They are associative collections Dict k v
indexed by key of type k with values of type v.

Their inner structure can be different, but they have a common interface:

insert       :: k -> v -> Dict k v -> Dict k v
maybeGet     :: k -> Dict k v -> Maybe v
getOrDefault :: k -> Dict k v -> v -> v
contains     :: k -> Dict k v -> Bool
delete       :: k -> Dict k v -> Dict k v
elems        :: Dict k v -> [v]
keys         :: Dict k v -> [k]
size         :: Dict k v -> Int
empty        :: Dict k v 
-}

-- 1. Populate a following typeclass
--    d denotes the actual type of the dictionary (its type constructor)   
class IDict d k v where
    insert       :: k -> v -> d k v -> d k v
    -- maybeGet     :: k -> d k v -> Maybe v
    -- getOrDefault :: k -> d k v -> v -> v
    -- contains     :: k -> d k v -> Bool
    -- delete       :: k -> d k v -> d k v
    -- elems        :: d k v -> [v]
    -- keys         :: d k v -> [k]
    -- size         :: d k v -> Int
    empty        :: d k v 

-- 2. Propose a naive implementation of the typeclass above 
data Dict k v = Dict [(k, v)]

instance IDict Dict Int Char where
    insert :: Int -> Char -> Dict Int Char -> Dict Int Char
    insert k v (Dict dict) = Dict (helpInsert k v dict) where 
        helpInsert k v [] = [(k, v)]
        helpInsert k v ((x, y):xs)| k == x    = (k, v) : xs 
                                | otherwise = (x, y) : helpInsert k v xs

    empty :: Dict Int Char
    empty = Dict []

-- 3. Implement the Show typeclass to represent your dictionary 
--    in the form {key : value} 
instance (Show k, Show v) => Show (Dict k v) where
    show :: Dict k v -> String
    show (Dict []) = ""
    show (Dict ((k, v):xs)) = "{" ++ show k ++ " : " ++ show v ++ "} " ++ show (Dict xs)

main = do
    print $ fromPairs kvPairs 
    where 
        kvPairs   = [(1,'h'), (2,'e'), (3, 'l'), (4,'l'), (5, 'o')] :: [(Int, Char)]
        -- kvPairs   = [(1,'h')] :: [(Int, Char)]
        -- kvPairs   = [] :: [(Int, Char)]
        fromPairs = foldl insert' (empty :: Dict Int Char)
        insert' dict (k, v) = insert k v dict