module Brainscrambler
       ( eval
       ) where

import           Universum

import           Brainscrambler.Interpreter (run)
import           Brainscrambler.Parser      (parse)

eval :: String -> String
eval = fromMaybe "" . fmap run . parse
