module Brainscrambler.Interpreter
       ( runBrainscrambler
       ) where

import           Universum

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
    { _cluster      :: Cluster StackId (Stack Int)
    , _cyclesStarts :: Stack (Program ())
    }

type Program
    = StateT ProgramState
    $ Writer String

Lens.makeLenses ''ProgramState

runBrainscrambler :: Brainscrambler () -> String
runBrainscrambler = eval . compile

eval :: Program () -> String
eval = execWriter . flip evalStateT emptyState
  where
    emptyState = ProgramState{..}
    _cluster = mempty
    _cyclesStarts = mempty

compile :: Brainscrambler () -> Program ()
compile (Pure a) = pure a
compile (Free as) = case as of
    Increment       next -> withNext next $ clusterValueHead %= succ
    Decrement       next -> withNext next $ clusterValueHead %= pred
    PushZero        next -> withNext next $ clusterValue %= Stack.push 0
    Input x         next -> withNext next $ clusterValue %= Stack.push x
    Pop             next -> withNext next $ clusterValue %= Stack.pop
    Rotate          next -> withNext next $ cluster %= Cluster.rotateRight
    MoveHeadToLeft  next -> withNext next $ cluster %= moveStackHeadToLeft
    MoveHeadToRight next -> withNext next $ cluster %= moveStackHeadToRight
    Output          next -> withNext next $ preuse clusterValueHead >>= mapM (tell . show)
    CycleStart      next -> withNext next $ cyclesStarts %= Stack.push (compile next)
    CycleEnd        next -> do
        head'' <- preuse clusterValueHead
        start' <- preuse (cyclesStarts . Stack._head)
        case (head'', start') of
            (Just head', Just start) | head' > 0 -> start
            _                        -> compile next
  where
    withNext next m = m >> compile next
    clusterValue :: Lens' ProgramState (Stack Int)
    clusterValue = cluster . Cluster.value
    clusterValueHead :: Traversal' ProgramState Int
    clusterValueHead = clusterValue . Stack._head

makeStackHeadMover
    :: (Cluster StackId (Stack a) -> Cluster StackId (Stack a))
    -> (Cluster StackId (Stack a) -> Cluster StackId (Stack a))
    -> (Cluster StackId (Stack a) -> Cluster StackId (Stack a))
makeStackHeadMover to from cluster' = fromMaybe cluster' $ do
    x <- cluster' ^? Cluster.value . Stack._head
    pure . from . (Cluster.value %~ Stack.push x) . to $ cluster'

moveStackHeadToRight :: Cluster StackId (Stack a) -> Cluster StackId (Stack a)
moveStackHeadToRight = makeStackHeadMover Cluster.rotateRight Cluster.rotateLeft

moveStackHeadToLeft :: Cluster StackId (Stack a) -> Cluster StackId (Stack a)
moveStackHeadToLeft = makeStackHeadMover Cluster.rotateLeft Cluster.rotateRight
