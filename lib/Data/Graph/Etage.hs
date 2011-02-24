{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Etage (
  shortestPaths
) where

import Control.Monad.State
import Data.Data
import Data.Graph.Inductive hiding (inn, inn', out, out', node', nodes, run)
import qualified Data.Map as M
import Data.Map hiding (filter, map, empty)
import Data.Tuple
import Control.Etage

type SPath b = (LPath b, b)
type SPaths a b = M.Map Node (a, SPath b) -- node is destination, last element of SPath

shortestPaths :: (DynGraph gr, Show a, Data a, Data b, Real b, Bounded b) => Node -> gr a b -> Incubation (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive)
shortestPaths from graph = do
  nodes <- ufoldM' growGraph M.empty graph
  let n = nodes ! from -- it is an error to try to get shortest paths to a nonexistent node
  sendTopologyChange n
  return n

growGraph :: forall a b. (Show a, Data a, Data b, Real b, Bounded b) => Context a b -> M.Map Node (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive) -> Incubation (M.Map Node (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive))
growGraph (inn, node, label, out) nodes = do
  -- TODO: Add assertion check for node not being in nodes
  nodeNerve <- (growNeuron :: NerveBoth (NodeNeuron a b)) (\o -> o { lnode = (node, label), linedges = inn' })
  mapM_ ((`attachTo` [TranslatableFor nodeNerve]) . (nodes !) . snd) inn'
  nodeNerve `attachTo` map (TranslatableFor . (nodes !) . snd) out'
  return $ insert node nodeNerve nodes
    where inn' = filter ((node /=) . snd) $ inn -- we ignore loopbacks
          out' = filter ((node /=) . snd) $ out -- we ignore loopbacks

-- TODO: Also make functions to manipulate graph
sendTopologyChange :: Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive -> Incubation ()
sendTopologyChange nerve = liftIO $ do
  time <- getCurrentImpulseTime
  sendForNeuron nerve $ TopologyChange time

data NodeState a b = NodeState {
    lastTopologyChangeTimestamp :: ImpulseTime,
    currentPaths :: SPaths a b
  }

type NodeIO a b = StateT (NodeState a b) IO

data NodeNeuron a b = NodeNeuron Node a (M.Map Node b) deriving (Typeable, Data)

deriving instance Typeable1 LPath
deriving instance Data a => Data (LPath a)

data GraphImpulse a b = TopologyUpdate {
    impulseTimestamp :: ImpulseTime,
    originator :: LNode a,
    destination :: LNode a,
    path :: SPath b
  } |
  TopologyChange {
    impulseTimestamp :: ImpulseTime
  } deriving (Eq, Ord, Show, Typeable, Data)

instance (Show a, Typeable a, Show b, Typeable b, Real b, Bounded b) => Impulse (GraphImpulse a b) where
  impulseTime TopologyUpdate { impulseTimestamp } = impulseTimestamp
  impulseTime TopologyChange { impulseTimestamp } = impulseTimestamp
  impulseValue TopologyUpdate { originator, path } = (toRational o) : (value . fst $ path)
    where (o, _) = originator
          value (LP p) = concatMap (\(n, l) -> [toRational n, toRational l]) p
  impulseValue TopologyChange {} = []

instance (Show a, Data a, Show b, Data b, Real b, Bounded b) => Neuron (NodeNeuron a b) where
  type NeuronFromImpulse (NodeNeuron a b) = GraphImpulse a b
  type NeuronForImpulse (NodeNeuron a b) = GraphImpulse a b
  data NeuronOptions (NodeNeuron a b) = NodeOptions {
      lnode :: LNode a,
      linedges :: Adj b
    } deriving (Eq, Ord, Read, Show) -- TODO: Derive Data when it will work

  grow NodeOptions { lnode = (node, label), linedges } = return $ NodeNeuron node label linedges'
    where linedges' = fromList . map swap $ linedges
  
  live nerve neuron@(NodeNeuron node label _) = evalStateT (run nerve neuron) (NodeState 0 (singleton node (label, (LP [(node, 0)], 0))))

run :: (Data b, Real b, Bounded b) => Nerve (GraphImpulse a b) fromConductivity (GraphImpulse a b) forConductivity -> NodeNeuron a b -> NodeIO a b ()
run nerve (NodeNeuron node label inedges) = forever $ do
  i <- liftIO $ getForNeuron nerve
  case i of
    TopologyChange { impulseTimestamp } -> do
      lastTimestamp <- gets lastTopologyChangeTimestamp
      when (impulseTimestamp > lastTimestamp) $ do
        modify (\s -> s { lastTopologyChangeTimestamp = impulseTimestamp })
        paths <- gets currentPaths
        liftIO $ do
          sendFromNeuron nerve i
          t <- getCurrentImpulseTime
          forM_ (toList paths) $ \(n, (l, p)) -> sendFromNeuron nerve TopologyUpdate { impulseTimestamp = t, originator = (node, label), destination = (n, l), path = p }
    TopologyUpdate { impulseTimestamp, originator = (o, _), destination = (d, l), path = (LP path, cost) } -> do
      -- TODO: Add assertion which checks if path length is really same as cost
      paths <- gets currentPaths
      let (_, (_, c)) = findWithDefault (undefined, (undefined, maxBound)) d paths
          ocost = inedges ! o
          cost' = cost + ocost
      when (cost' < c) $ do
        let path' = LP $ (o, ocost) : path
            paths' = insert d (l, (path', cost')) paths
        modify (\s -> s { currentPaths = paths' })
        liftIO $ sendFromNeuron nerve TopologyUpdate { impulseTimestamp, originator = (node, label), destination = (d, l), path = (path', cost') }

ufoldM' :: (Graph gr, Monad m) => (Context a b -> c -> m c) -> c -> gr a b -> m c
ufoldM' f u g | isEmpty g = return u
              | otherwise = ufoldM' f u g' >>= \u' -> f c u'
                  where (c, g') = matchAny g

{-
gmapM' :: (DynGraph gr, Monad m) => (Context a b -> m (Context c d)) -> gr a b -> m (gr c d)
gmapM' f = ufoldM' (\c u -> f c >>= \c' -> return $ c' & u) empty
-}

