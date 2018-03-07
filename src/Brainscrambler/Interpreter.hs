module Brainscrambler.Interpreter
       ( runBrainscrambler
       ) where

import           Universum

import           Control.Monad.Extra    ((<<))
import           Control.Monad.Free     (Free (..))
import           Control.Monad.Writer   (Writer, execWriter, tell)

import           Lens.Micro.Platform    ((%=))
import qualified Lens.Micro.Platform    as Lens

import           Brainscrambler.AST     (Brainscrambler, BrainscramblerF (..))

import           Brainscrambler.Stack   (Stack)
import qualified Brainscrambler.Stack   as Stack

import           Brainscrambler.Cluster (Cluster)
import qualified Brainscrambler.Cluster as Cluster

data StackId
    = StackA
    | StackB
    | StackC
    deriving (Eq, Ord, Bounded, Enum, Show)

data ProgramState = ProgramState
    { _cluster :: Cluster StackId (Stack Int)
    , _labels  :: Stack (Program ())
    }

type Program
    = StateT ProgramState
    $ Writer String

Lens.makeLenses ''ProgramState

runBrainscrambler :: Brainscrambler () -> String
runBrainscrambler = interpret . compile

interpret :: Program () -> String
interpret = execWriter . flip evalStateT emptyState
  where
    emptyState = ProgramState{..}
    _cluster = Cluster.makeBy (\_ -> Stack.fromList [0])
    _labels = Stack.fromList []

compile :: Brainscrambler () -> Program ()
compile (Pure a) = pure a
compile (Free as) = case as of
    Increment       next -> compile next << clusterValueHead %= succ
    Decrement       next -> compile next << clusterValueHead %= pred
    PushZero        next -> compile next << clusterValue %= Stack.push 0
    Input x         next -> compile next << clusterValue %= Stack.push x
    Pop             next -> compile next << clusterValue %= Stack.pop
    Rotate          next -> compile next << cluster %= Cluster.rotateRight
    MoveHeadToLeft  next -> compile next << cluster %= moveStackHeadToLeft
    MoveHeadToRight next -> compile next << cluster %= moveStackHeadToRight
    Output          next -> compile next << (preuse clusterValueHead >>= mapM (tell . show))
    CycleStart      next -> compile next << labels %= Stack.push (compile next)
    CycleEnd        next -> do
        head'' <- preuse clusterValueHead
        start' <- preuse lastLabel
        case (head'', start') of
            (Just head', Just start) | head' > 0 -> start
            _                        -> compile next << labels %= Stack.pop
  where
    clusterValue :: Lens' ProgramState (Stack Int)
    clusterValue = cluster . Cluster.value
    clusterValueHead :: Traversal' ProgramState Int
    clusterValueHead = clusterValue . Stack._head
    lastLabel :: Traversal' ProgramState (Program ())
    lastLabel = labels . Stack._head

makeStackHeadMover
    :: (Cluster StackId (Stack a) -> Cluster StackId (Stack a))
    -> (Cluster StackId (Stack a) -> Cluster StackId (Stack a))
    -> (Cluster StackId (Stack a) -> Cluster StackId (Stack a))
makeStackHeadMover to from cluster' = fromMaybe cluster' $ do
    head' <- cluster' ^? Cluster.value . Stack._head
    pure $ cluster'
        & Cluster.value %~ Stack.pop
        & to
        & Cluster.value %~ Stack.push head'
        & from

moveStackHeadToRight :: Cluster StackId (Stack a) -> Cluster StackId (Stack a)
moveStackHeadToRight = makeStackHeadMover Cluster.rotateRight Cluster.rotateLeft

moveStackHeadToLeft :: Cluster StackId (Stack a) -> Cluster StackId (Stack a)
moveStackHeadToLeft = makeStackHeadMover Cluster.rotateLeft Cluster.rotateRight
