module Gadgets.Counter where

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)

type Counter a = M.Map a Integer

-- | Add the value at the key by n.
addN :: Ord a => a -> Integer -> Counter a -> Counter a
addN _ 0 = id
addN e n = M.insertWith (const (+ n)) e n

-- | Remove the key from the "Counter".
clear :: Ord a => a -> Counter a -> Counter a
clear = M.delete

-- | The empty "Counter".
empty :: Counter a
empty = M.empty

-- | Groups equal elements in a list together, returning a list of pairs where
-- The second element represents the count of the first element in the original
-- list.
group :: Ord a => [a] -> [(a, Integer)]
group = toPairList . fromList

-- | Generates a "Counter" from a list.
fromList :: Ord a => [a] -> Counter a
fromList = foldr inc empty

-- | Increment the value at the key.
inc :: Ord a => a -> Counter a -> Counter a
inc = flip (M.insertWith (const (+ 1))) 1

-- | Returns all keys of the "Counter".
keys :: Counter a -> [a]
keys = M.keys

-- | Convert a "Counter" to pairs of elements and counts.
toPairList :: Counter a -> [(a, Integer)]
toPairList = M.toList

infixl 9 !, !?

-- | Finds the value of the given key, or zero if key is not found.
(!) :: Ord a => Counter a -> a -> Integer
(!) = (fromMaybe 0 .) . (M.!?)

-- | Finds the value of the given key.
(!?) :: Ord a => Counter a -> a -> Maybe Integer
(!?) = (M.!?)
