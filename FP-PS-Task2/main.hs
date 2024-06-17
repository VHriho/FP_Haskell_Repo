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
    maybeGet     :: k -> d k v -> Maybe v
    -- getOrDefault :: k -> d k v -> v -> v
    contains     :: k -> d k v -> Bool
    delete       :: k -> d k v -> d k v
    elems        :: d k v -> [v]
    keys         :: d k v -> [k]
    size         :: d k v -> Int
    empty        :: d k v 

main = do
    print "1. Populated typeclass"