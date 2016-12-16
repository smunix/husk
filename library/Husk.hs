-- library/Husk.hs
module Husk ( husk
            , ordered
            , insertOrdered
            ) where

import Data.List (sort)

{- |
    An alias for the unit value.

    >>> husk
    ()
-}
husk :: () -- ^ The unit type.
husk = ()

-- | Check that the input list is ordered.
ordered ::
  (Ord a)
  => [a] -- ^ A list of elements.
  -> Bool
ordered xs = all (uncurry (<=)) (zip xs (drop 1 xs))

-- | Insert an element in an ordered list.
insertOrdered ::
  (Ord a)
  => a -- ^ An element to insert.
  -> [a] -- ^ A list of elements
  -> [a]
insertOrdered x xs =  (takeWhile (<x) oxs)
                      ++ [x]
                      ++ (dropWhile (<x) oxs)
  where
    oxs = sort xs
