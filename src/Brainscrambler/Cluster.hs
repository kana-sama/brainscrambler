module Brainscrambler.Cluster
       ( Cluster
       , value
       , rotateLeft
       , rotateRight
       ) where

import           Universum

import           Data.Enum.Extra     (closuredPred, closuredSucc, enums)

import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Lens.Micro.Platform ((?~))
import qualified Lens.Micro.Platform as Lens

data Cluster i a = Cluster
    { _values  :: Map i a
    , _valueId :: i
    }

Lens.makeLenses ''Cluster

instance (Ord i, Bounded i, Enum i, Monoid a) => Monoid (Cluster i a) where
    mempty = Cluster{..}
      where
        _valueId = minBound
        _values = Map.fromList $ (, mempty) <$> enums
    mappend = error "no append for `Cluster i a`"

value :: (Ord i, Monoid a) => Lens' (Cluster i a) a
value = Lens.lens getter setter
  where
    getter cluster =
        let id = cluster ^. valueId
            value' = cluster ^. values . Lens.at id
        in fromMaybe mempty value'
    setter cluster value' =
        let id = cluster ^. valueId
        in cluster & values . Lens.at id ?~ value'

rotateLeft :: (Eq i, Enum i, Bounded i) => Cluster i a -> Cluster i a
rotateLeft = valueId %~ closuredPred

rotateRight :: (Eq i, Enum i, Bounded i) => Cluster i a -> Cluster i a
rotateRight = valueId %~ closuredSucc
