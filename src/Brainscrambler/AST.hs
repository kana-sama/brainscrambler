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

import           Universum

import           Control.Monad.Free    (Free, MonadFree, liftF)
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
    deriving (Functor)

type Brainscrambler = Free BrainscramblerF

makeFree ''BrainscramblerF
