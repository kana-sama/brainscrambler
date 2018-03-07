module Brainscrambler.AST
       ( BrainscramblerF(..)
       , Brainscrambler
       , increment
       , decrement
       , pushZero
       , pop
       , input
       , output
       , rotate
       , moveHeadToLeft
       , moveHeadToRight
       , cycleStart
       , cycleEnd
       ) where

import           Universum             hiding (Show (..), show)

import           Text.Show

import           Control.Monad.Free    (Free, MonadFree, iter, liftF)
import           Control.Monad.Free.TH (makeFree)

data BrainscramblerF a
    = Increment a
    | Decrement a
    | PushZero a
    | Pop a
    | Input Int a
    | Output a
    | Rotate a
    | MoveHeadToLeft a
    | MoveHeadToRight a
    | CycleStart a
    | CycleEnd a
    deriving (Functor, Foldable)

type Brainscrambler = Free BrainscramblerF

makeFree ''BrainscramblerF

instance {-# OVERLAPS #-} Show (Brainscrambler ()) where
    show ast = iter go ("" <$ ast)
      where
        go (Increment next)       = "+" ++ next
        go (Decrement next)       = "-" ++ next
        go (PushZero next)        = "*" ++ next
        go (Pop next)             = "^" ++ next
        go (Input x next)         = "," ++ show x ++ next
        go (Output next)          = "." ++ next
        go (Rotate next)          = "#" ++ next
        go (MoveHeadToLeft next)  = "<" ++ next
        go (MoveHeadToRight next) = ">" ++ next
        go (CycleStart next)      = "[" ++ next
        go (CycleEnd next)        = "]" ++ next

