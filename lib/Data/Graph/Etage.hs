{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Please read the "Control.Etage" framework documentation for general information how it works. Also check included @test@ program for
an example of how to work with the algorithms bellow.
-}

module Data.Graph.Etage (
  shortestPaths,
  sendTopologyChange,
  GraphImpulse(..)
) where

import Control.Exception
import Control.Monad.State
import Data.Data
import Data.Graph.Inductive hiding (inn, inn', out, out', node', nodes, run)
import qualified Data.Map as M
import Data.Map hiding (filter, map, empty, null, lookup, foldl)
import Data.Tuple
import System.IO

import Control.Etage

type SPath b = (LPath b, b)
type SPaths a b = M.Map Node (a, SPath b) -- node is destination, last element of SPath

{-|
Shortest paths algorithm (from all to all nodes) using message ('Impulse's in the "Control.Etage" terminology) passing between the
nodes along the edges of the graph to compute shortest paths. Loosely based on the algorithm used in the Babel routing
protocol, <http://www.pps.jussieu.fr/~jch/software/babel/>.

It takes a "Data.Graph.Inductive" graph as an input and produces a map between source nodes and its corresponding 'Nerve's, over which
'Impulse's about shortest paths search will be send. To trigger the search 'sendTopologyChange' should be used on returned 'Nerve's.

One way how to collect this 'Impulse's into an array for querying about shortest paths can be found in the @test@ program found in
this package.

While shortest paths search is lasting, information about suboptimal paths is already available. This algorithm also allows effective
incremental search after graph topology changes (new nodes are added or removed, weights are changed) but this is not yet implemented.
-}
shortestPaths :: (DynGraph gr, Show a, Show b, Data a, Data b, Real b, Bounded b) => gr a b -> Incubation (M.Map Node (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive))
shortestPaths = ufoldM' growGraph M.empty

growGraph :: forall a b. (Show a, Show b, Data a, Data b, Real b, Bounded b) => Context a b -> M.Map Node (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive) -> Incubation (M.Map Node (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive))
growGraph (inn, node, label, out) nodes = do
  -- TODO: Sometimes nerve is not connected in both directions, how to fix memory leak then?
  liftIO $ do
    assertIO $ node `notMember` nodes
    assertIO $ all ((`member` nodes) . snd) inn'
    assertIO $ all ((`member` nodes) . snd) out'
  nodeNerve <- (growNeuron :: NerveBoth (NodeNeuron a b)) (\o -> o { lnode = (node, label) })
  mapM_ ((`attachTo` [TranslatableFor nodeNerve]) . (nodes !) . snd) out'
  nodeNerve `attachTo` map (TranslatableFor . (nodes !) . snd) inn'
  liftIO $ do
    time <- getCurrentImpulseTime
    unless (null out') $ sendForNeuron nodeNerve $ AddOutEdges time out'
    mapM_ (\(l, n) -> sendForNeuron (nodes ! n) $ AddOutEdges time [(l, node)]) inn'
  return $ insert node nodeNerve nodes
    where inn' = filter ((node /=) . snd) inn -- we ignore loopbacks
          out' = filter ((node /=) . snd) out -- we ignore loopbacks

-- TODO: Also make functions to manipulate graph
-- TODO: We have to send TopologyChange to all nodes because currently it is not propagated correctly around (just along inbound edges, but it should along all)
{-|
Inform nodes that topology has changed (new nodes have been added or removed, weights changed).

Currently it should only be invoked after the data-flow graph structure has been built (for example with 'shortestPaths'). As
graph topology changing interface (and thus incremental nature of algorithms) is not yet implemented.
-}
sendTopologyChange :: M.Map Node (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive) -> Incubation ()
sendTopologyChange nodes = liftIO $ do
  time <- getCurrentImpulseTime
  forM_ (elems nodes) $ \n ->
    sendForNeuron n $ TopologyChange time

data NodeState a b = NodeState {
    lastTopologyChangeTimestamp :: ImpulseTime,
    currentPaths :: SPaths a b,
    outedges :: M.Map Node b
  }

type NodeIO a b = StateT (NodeState a b) IO

data NodeNeuron a b = NodeNeuron Node a deriving (Typeable, Data)

deriving instance Typeable1 LPath
deriving instance Data a => Data (LPath a)

data GraphImpulse a b = TopologyUpdate {
    impulseTimestamp :: ImpulseTime,
    originator :: LNode a,
    destination :: LNode a,
    path :: SPath b
  } -- ^ Informs nodes about possible improvement in the topology information, like a newly discovered shortest path.
  | TopologyChange {
    impulseTimestamp :: ImpulseTime
  } -- ^ Informs nodes that topology has changed and the algorithm should be triggered (again).
  | AddOutEdges {
    impulseTimestamp :: ImpulseTime,
    newOutEdges :: Adj b
  } -- ^ Inform the node that new outbound edges have been 'attach'ed to it, giving the node their weights.
  deriving (Eq, Ord, Show, Typeable, Data)

instance (Show a, Typeable a, Show b, Typeable b, Real b, Bounded b) => Impulse (GraphImpulse a b) where
  impulseTime TopologyUpdate { impulseTimestamp } = impulseTimestamp
  impulseTime TopologyChange { impulseTimestamp } = impulseTimestamp
  impulseTime AddOutEdges { impulseTimestamp } = impulseTimestamp
  impulseValue TopologyUpdate { originator = (o, _), path } = toRational o : (value . fst $ path)
    where value (LP p) = concatMap (\(n, l) -> [toRational n, toRational l]) p
  impulseValue TopologyChange {} = []
  impulseValue AddOutEdges { newOutEdges } = concatMap value newOutEdges
    where value (l, n) = [toRational l, toRational n]

instance (Show a, Data a, Show b, Data b, Real b, Bounded b) => Neuron (NodeNeuron a b) where
  type NeuronFromImpulse (NodeNeuron a b) = GraphImpulse a b
  type NeuronForImpulse (NodeNeuron a b) = GraphImpulse a b
  data NeuronOptions (NodeNeuron a b) = NodeOptions {
      lnode :: LNode a
    } deriving (Eq, Ord, Read, Show) -- TODO: Derive Data when it will work

  mkDefaultOptions = return NodeOptions {
      lnode = undefined
    }

  grow NodeOptions { lnode = (node, label) } = return $ NodeNeuron node label
  
  live nerve neuron@(NodeNeuron node label) = evalStateT (run nerve neuron) (NodeState 0 (singleton node (label, (LP [(node, 0)], 0))) M.empty)

run :: (Data b, Real b, Bounded b) => Nerve (GraphImpulse a b) fromConductivity (GraphImpulse a b) forConductivity -> NodeNeuron a b -> NodeIO a b ()
run nerve (NodeNeuron node label) = forever $ do
  -- TODO: We could process multiple impulses at the same time, so that we do not send out updates for paths where better paths are already queued for us
  impulse <- liftIO $ getForNeuron nerve
  case impulse of
    TopologyChange { impulseTimestamp } -> do
      lastTimestamp <- gets lastTopologyChangeTimestamp
      when (impulseTimestamp > lastTimestamp) $ do
        modify (\s -> s { lastTopologyChangeTimestamp = impulseTimestamp })
        paths <- gets currentPaths
        liftIO $ do
          sendFromNeuron nerve impulse
          t <- liftIO getCurrentImpulseTime
          -- TODO: TopologyChange should be propagated correctly (along all edges and not just along inbound edges, as it is now)
          forM_ (toList paths) $ \(n, (l, p)) ->
            sendFromNeuron nerve TopologyUpdate { impulseTimestamp = t, originator = (node, label), destination = (n, l), path = p }
    TopologyUpdate { impulseTimestamp, originator = (o, _), destination = (d, l), path = (LP path, cost) } -> do
      liftIO $ do
        assertIO $ abs (cost - (sum . map snd $ path)) * 100000 < 1 -- we have to do compare it like that to account for approximate nature of float values
        assertIO $ (fst . last $ path) == d
      out <- gets outedges
      case M.lookup o out of
        Nothing    -> liftIO $ hPutStrLn stderr "Warning: TopologyUpdate message arrived before AddOutEdges message."
        Just ocost -> do
          paths <- gets currentPaths
          let (_, (_, c)) = findWithDefault (undefined, (undefined, maxBound)) d paths
              cost' = cost + ocost
          when (cost' < c) $ do
            let path' = LP $ (node, ocost) : path
                paths' = insert d (l, (path', cost')) paths
            modify (\s -> s { currentPaths = paths' })
            liftIO $ sendFromNeuron nerve TopologyUpdate { impulseTimestamp, originator = (node, label), destination = (d, l), path = (path', cost') }
    AddOutEdges { newOutEdges } -> do
      out <- gets outedges
      let out' = foldl (\i (l, n) -> insert n l i) out newOutEdges
      modify (\s -> s { outedges = out' })

ufoldM' :: (Graph gr, Monad m) => (Context a b -> c -> m c) -> c -> gr a b -> m c
ufoldM' f u g | isEmpty g = return u
              | otherwise = ufoldM' f u g' >>= \u' -> f c u'
                  where (c, g') = matchAny g

{-
gmapM' :: (DynGraph gr, Monad m) => (Context a b -> m (Context c d)) -> gr a b -> m (gr c d)
gmapM' f = ufoldM' (\c u -> f c >>= \c' -> return $ c' & u) empty
-}

instance Bounded Float where
  minBound = -1/0
  maxBound = 1/0

instance Bounded Double where
  minBound = -1/0
  maxBound = 1/0

assertIO :: Bool -> IO ()
assertIO cond = evaluate (assert cond ())
