module Brainscrambler.Stack
       ( Stack
       , push
       , pop
       , _head
       ) where

import           Universum

import qualified Lens.Micro.Platform as Lens

newtype Stack a = Stack { _stackList :: [a] }

Lens.makeLenses ''Stack

instance Monoid (Stack a) where
    mempty = Stack []
    mappend (Stack a) (Stack b) = Stack (mappend a b)

_head :: Traversal' (Stack a) a
_head = stackList . Lens._head

push :: a -> Stack a -> Stack a
push x = stackList %~ (x :)

pop :: Stack a -> Stack a
pop = stackList %~ safeTail
  where
    safeTail []     = []
    safeTail (_:xs) = xs
