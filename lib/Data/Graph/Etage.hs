{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.Etage (
  shortestPaths
) where

import Control.Applicative hiding (empty)
import Control.Monad.Trans
import Data.Data
import Data.Graph.Inductive hiding (inn, inn', out, out', node')
import Data.Graph.Inductive.Internal.RootPath
import Data.Maybe
import Control.Etage

shortestPaths :: (DynGraph gr, Show a, Data a, Data b, Real b) => Node -> gr a b -> Incubation (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive)
shortestPaths from graph = do
  ngraph <- gmapM' growGraph graph
  return . fromJust $ lab ngraph from -- it is an error to try to get shortest paths to a nonexistent node

growGraph :: forall a b. (Show a, Data a, Data b, Real b) => Context a b -> Incubation (Context (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive) (Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive))
growGraph (inn, node, label, out) = do
  nodeNerve <- (growNeuron :: NerveBoth (NodeNeuron a b)) (\o -> o { lnode = (node, label) })
  inn' <- concat <$> mapM (growEdge swap) inn
  out' <- concat <$> mapM (growEdge id) out
  mapM_ ((`attachTo` [TranslatableFor nodeNerve]) . fst) inn'
  nodeNerve `attachTo` (map (TranslatableFor . fst) out')
  sendTopologyChange nodeNerve
  return (inn', node, nodeNerve, out')
    where growEdge f (weight, node') | node' == node = return [] -- we do not grow loopbacks
                                     | otherwise = do
                                         edgeNerve <- (growNeuron :: NerveBoth (EdgeNeuron a b)) (\o -> o { ledge = f (node, node', weight) })
                                         return [(edgeNerve, node')]
          swap (n, n', w) = (n', n, w)

sendTopologyChange :: Nerve (GraphImpulse a b) AxonConductive (GraphImpulse a b) AxonConductive -> Incubation ()
sendTopologyChange nerve = liftIO $ do
  time <- getCurrentImpulseTime
  sendForNeuron nerve $ TopologyChange time

data NodeNeuron a b = NodeNeuron Node a deriving (Typeable, Data)

deriving instance Typeable1 LPath
deriving instance Data a => Data (LPath a)

data GraphImpulse a b = Originator {
    impulseTimestamp :: ImpulseTime,
    originator :: LNode a,
    paths :: LRTree b
  } |
  TopologyChange {
    impulseTimestamp :: ImpulseTime
  } deriving (Eq, Ord, Show, Typeable, Data)

instance (Show a, Typeable a, Show b, Typeable b, Real b) => Impulse (GraphImpulse a b) where
  impulseTime Originator { impulseTimestamp } = impulseTimestamp
  impulseTime TopologyChange { impulseTimestamp } = impulseTimestamp
  impulseValue Originator { originator, paths } = (toRational o) : (concatMap value paths)
    where (o, _) = originator
          value (LP p) = concatMap (\(n, l) -> [toRational n, toRational l]) p
  impulseValue TopologyChange {} = []

instance (Show a, Data a, Show b, Data b, Real b) => Neuron (NodeNeuron a b) where
  type NeuronFromImpulse (NodeNeuron a b) = GraphImpulse a b
  type NeuronForImpulse (NodeNeuron a b) = GraphImpulse a b
  data NeuronOptions (NodeNeuron a b) = NodeOptions {
      lnode :: LNode a
    } deriving (Eq, Ord, Read, Show) -- TODO: Derive Data when it will work

  grow NodeOptions { lnode = (node, label) } = return $ NodeNeuron node label
  
  live nerve (NodeNeuron node label) = return ()

data EdgeNeuron a b = EdgeNeuron Edge b deriving (Typeable, Data)

instance (Show a, Data a, Show b, Data b, Real b) => Neuron (EdgeNeuron a b) where
  type NeuronFromImpulse (EdgeNeuron a b) = GraphImpulse a b
  type NeuronForImpulse (EdgeNeuron a b) = GraphImpulse a b
  data NeuronOptions (EdgeNeuron a b) = EdgeOptions {
      ledge :: LEdge b
    } deriving (Eq, Ord, Read, Show) -- TODO: Derive Data when it will work

  grow EdgeOptions { ledge = (node1, node2, weight) } = return $ EdgeNeuron (node1, node2) weight
  
  live nerve (EdgeNeuron edge weight) = forever $ do
    i <- getForNeuron nerve
    sendFromNeuron nerve $ case i of
                             TopologyChange {} -> i
                             Originator { impulseTimestamp, originator, paths } -> Originator { impulseTimestamp, originator, paths' }
                               where paths' = map

ufoldM' :: (Graph gr, Monad m) => (Context a b -> c -> m c) -> c -> gr a b -> m c
ufoldM' f u g | isEmpty g = return u
              | otherwise = ufoldM' f u g' >>= \u' -> f c u'
                  where (c, g') = matchAny g

gmapM' :: (DynGraph gr, Monad m) => (Context a b -> m (Context c d)) -> gr a b -> m (gr c d)
gmapM' f = ufoldM' (\c u -> f c >>= \c' -> return $ c' & u) empty
