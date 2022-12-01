module Gadgets.Maybe where

import            Control.Monad (guard)

-- | Applies a predicate, returning "Nothing" if False.
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f a = guard (f a) >> Just a
