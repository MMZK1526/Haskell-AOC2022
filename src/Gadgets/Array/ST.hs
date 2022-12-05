{-# LANGUAGE FlexibleContexts #-}

module Gadgets.Array.ST where

import           Control.Monad.ST (ST)
import           Data.Array (Array, Ix)
import qualified Data.Array.ST as A
import qualified Data.Array.Unsafe as A
import qualified Gadgets.Array as A

type STArray = A.STArray

-- | Making an array from a list, indexed from 0.
fromList :: [e] -> ST s (STArray s Int e)
fromList = thaw . A.fromList

-- | Making an array from a bound and a default value.
newArray :: Ix i => (i, i) -> a -> ST s (STArray s i a)
newArray = A.newArray

-- | This is the same as the default @freeze@ function, but it has specified
-- type to avoid explicit signature binding.
freeze :: Ix i => STArray s i a -> ST s (Array i a)
freeze = A.freeze

-- | This is the same as the default @thaw@ function, but it has specified type
-- to avoid explicit signature binding.
thaw :: Ix i => Array i a -> ST s (STArray s i a)
thaw = A.thaw

-- | This is the same as the default @unsafeFreeze@ function, but it has
-- specified type to avoid explicit signature binding.
unsafeFreeze :: Ix i => STArray s i a -> ST s (Array i a)
unsafeFreeze = A.unsafeFreeze

-- | This is the same as the default @unsafeThaw@ function, but it has specified
-- type to avoid explicit signature binding.
unsafeThaw :: Ix i => Array i a -> ST s (STArray s i a)
unsafeThaw = A.unsafeThaw
