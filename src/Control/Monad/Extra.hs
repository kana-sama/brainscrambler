module Control.Monad.Extra
       ( (<<)
       ) where

import           Universum

infixr 1 <<

(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)
