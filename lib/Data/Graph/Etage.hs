{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Etage (
  shortestPaths,
  GraphImpulse(..)
) where

import Control.Exception
import Control.Monad.State
import Data.Data
import Data.Graph.Inductive hiding (inn, inn', out, out', node', nodes, run)
import qualified Data.Map as M
import Data.Map hiding (filter, map, empty, null, lookup)
import Data.Tuple
import Control.Etage
import System.IO

type SPath b = (LPath b, b)
type SPaths a b = M.Map Node (a, SPath b) -- node is destination, last element of SPath

shortestPaths :: (DynGraph gr, Show a, Data a, Data b, Real b, Bounded b) => Node -> gr a b -> Incubation (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive)
shortestPaths from graph = do
  nodes <- ufoldM' growGraph M.empty graph
  sendTopologyChange nodes
  return $ nodes ! from -- it is an error to try to get shortest paths to a nonexistent node

growGraph :: forall a b. (Show a, Data a, Data b, Real b, Bounded b) => Context a b -> M.Map Node (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive) -> Incubation (M.Map Node (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive))
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
    where inn' = filter ((node /=) . snd) $ inn -- we ignore loopbacks
          out' = filter ((node /=) . snd) $ out -- we ignore loopbacks

-- TODO: Also make functions to manipulate graph
-- TODO: We have to send TopologyChange to all nodes because currently it is not propagated correctly around (just along inbound edges, but it should along all)
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
  } |
  TopologyChange {
    impulseTimestamp :: ImpulseTime
  } |
  AddOutEdges {
    impulseTimestamp :: ImpulseTime,
    newOutEdges :: Adj b
  } deriving (Eq, Ord, Show, Typeable, Data)

instance (Show a, Typeable a, Show b, Typeable b, Real b, Bounded b) => Impulse (GraphImpulse a b) where
  impulseTime TopologyUpdate { impulseTimestamp } = impulseTimestamp
  impulseTime TopologyChange { impulseTimestamp } = impulseTimestamp
  impulseTime AddOutEdges { impulseTimestamp } = impulseTimestamp
  impulseValue TopologyUpdate { originator, path } = (toRational o) : (value . fst $ path)
    where (o, _) = originator
          value (LP p) = concatMap (\(n, l) -> [toRational n, toRational l]) p
  impulseValue TopologyChange {} = []
  impulseValue AddOutEdges { newOutEdges } = concatMap (\(l, n) -> [toRational l, toRational n]) newOutEdges

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
          forM_ (toList paths) $ \(n, (l, p)) -> do
            sendFromNeuron nerve $ TopologyUpdate { impulseTimestamp = t, originator = (node, label), destination = (n, l), path = p }
    TopologyUpdate { impulseTimestamp, originator = (o, _), destination = (d, l), path = (LP path, cost) } -> do
      liftIO $ do
        assertIO $ abs (cost - (sum . map snd $ path)) * 100000 < 1 -- we have to do compare it like that to account for approximate nature of float values
        assertIO $ (fst . last $ path) == d
      out <- gets outedges
      case M.lookup o out of
        Nothing    -> liftIO $ hPutStrLn stderr $ "Warning: TopologyUpdate message arrived before AddOutEdges message."
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
