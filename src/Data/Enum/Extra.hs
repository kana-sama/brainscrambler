module Data.Enum.Extra
       ( closedSucc
       , closedPred
       , enums
       ) where

import           Universum

closedSucc :: (Eq a, Enum a, Bounded a) => a -> a
closedSucc x
  | x == maxBound = minBound
  | otherwise     = succ x

closedPred :: (Eq a, Enum a, Bounded a) => a -> a
closedPred x
  | x == minBound = maxBound
  | otherwise     = pred x

enums :: (Enum a, Bounded a) => [a]
enums = enumFrom minBound
