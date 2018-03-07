module Brainscrambler.Cluster
       ( Cluster
       , makeBy
       , value
       , rotateLeft
       , rotateRight
       ) where

import           Universum

import           Data.Enum.Extra     (closedPred, closedSucc, enums)

import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Lens.Micro.Platform ((?~))
import qualified Lens.Micro.Platform as Lens

data Cluster i a = Cluster
    { _values  :: Map i a
    , _valueId :: i
    , _makeFn  :: i -> a
    }

Lens.makeLenses ''Cluster

makeBy :: (Ord i, Bounded i, Enum i) => (i -> a) -> Cluster i a
makeBy f = Cluster{..}
  where
    _valueId = minBound
    _values = Map.fromList . fmap (\i -> (i, f i)) $ enums
    _makeFn = f

value :: Ord i => Lens' (Cluster i a) a
value = Lens.lens getter setter
  where
    getter cluster = fromMaybe defaultValue value'
      where
        id = cluster ^. valueId
        value' = cluster ^. values . Lens.at id
        defaultValue = (cluster ^. makeFn) id
    setter cluster value' = cluster & values . Lens.at id ?~ value'
      where
        id = cluster ^. valueId

rotateLeft :: (Eq i, Enum i, Bounded i) => Cluster i a -> Cluster i a
rotateLeft = valueId %~ closedPred

rotateRight :: (Eq i, Enum i, Bounded i) => Cluster i a -> Cluster i a
rotateRight = valueId %~ closedSucc
