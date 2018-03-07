module Data.Enum.Extra
       ( closuredSucc
       , closuredPred
       , enums
       ) where

import           Universum

closuredSucc :: (Eq a, Enum a, Bounded a) => a -> a
closuredSucc x
  | x == maxBound = minBound
  | otherwise     = succ x

closuredPred :: (Eq a, Enum a, Bounded a) => a -> a
closuredPred x
  | x == minBound = maxBound
  | otherwise     = pred x

enums :: (Enum a, Bounded a) => [a]
enums = enumFrom minBound
