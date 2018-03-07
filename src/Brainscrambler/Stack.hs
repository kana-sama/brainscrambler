module Brainscrambler.Stack
       ( Stack
       , fromList
       , push
       , pop
       , _head
       ) where

import           Universum

import qualified Lens.Micro.Platform as Lens

newtype Stack a = Stack { _getList :: [a] }

Lens.makeLenses ''Stack

fromList :: [a] -> Stack a
fromList = Stack

_head :: Traversal' (Stack a) a
_head = getList . Lens._head

push :: a -> Stack a -> Stack a
push x = getList %~ (x :)

pop :: Stack a -> Stack a
pop = getList %~ safeTail
  where
    safeTail []     = []
    safeTail (_:xs) = xs
