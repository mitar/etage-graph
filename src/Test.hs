{-# LANGUAGE TypeFamilies, DeriveDataTypeable, NamedFieldPuns #-}

module Main (
  main
) where

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Data
import Data.Graph.Etage
import Data.Graph.Inductive hiding (nodes, edges)
import Data.List
import qualified Data.Map as M
import Data.Map hiding (filter, map, empty, null, lookup, insert)
import System.Environment
import System.Random
import Text.Printf

import Control.Etage

outputGraphFilename :: String
outputGraphFilename = "out.graph"

outputDotFilename :: String
outputDotFilename = "out.dot"

generateGraph :: Int -> IO (Gr String Double)
generateGraph graphSize = do
  when (graphSize < 1) $ throwIO $ AssertionFailed $ "Graph size out of bounds " ++ show graphSize
  let nodes = map (\n -> (n, show n)) [1..graphSize]
  edges <- fmap concat $ forM [1..graphSize] $ \node -> do
    nedges <- randomRIO (0, graphSize)
    others <- fmap (filter (node /=) . nub) $ forM [1..nedges] $ \_ -> randomRIO (1, graphSize)
    gen <- getStdGen
    let weights = randomRs (1, 10) gen
    return $ zip3 (repeat node) others weights
  return $ mkGraph nodes edges

data TestNeuron a b = TestNeuron Node (Gr a b) deriving (Typeable)

instance (Show a, Data a, Show b, Data b, Real b, Bounded b) => Neuron (TestNeuron a b) where
  type NeuronFromImpulse (TestNeuron a b) = NoImpulse
  type NeuronForImpulse (TestNeuron a b) = GraphImpulse a b
  data NeuronOptions (TestNeuron a b) = NodeOptions {
      sourceNode :: Node,
      graph :: Gr a b
    }

  mkDefaultOptions = return NodeOptions {
      sourceNode = undefined,
      graph = undefined
    }

  grow NodeOptions { sourceNode, graph } = return $ TestNeuron sourceNode graph
  
  live nerve (TestNeuron sourceNode graph) = waitAndTest M.empty
    where paths = fromList . map (\(LP (n@(node, len):nodes)) -> (node, (len, reverse . map fst $ n:nodes))) $ spTree sourceNode graph
          waitAndTest currentPaths = do
            impulse <- getForNeuron nerve
            case impulse of
              TopologyChange {}                                                 -> waitAndTest currentPaths
              AddOutEdges {}                                                    -> waitAndTest currentPaths
              TopologyUpdate { destination = (node, _), path = (LP path, len) } -> do
                let currentPaths' = M.insert node (len, map fst path) currentPaths
                    found = (fromIntegral . sum . map fromEnum . elems $ intersectionWith (\(l, p) (l', p') -> l == l' && p == p') paths currentPaths') / (fromIntegral $ size paths) :: Float
                putStrLn $ "Found " ++ (printf "%.2f %%" (found * 100)) ++ " shortest paths."
                when (found == 1.0) $ dissolving currentPaths'
                waitAndTest currentPaths'

main :: IO ()
main = do
  prepareEnvironment
  
  args <- getArgs
  
  graph <- if null args
             then do
               graph <- generateGraph 6
               putStrLn $ "Writing graph to \"" ++ outputGraphFilename ++ "\"."
               writeFile outputGraphFilename $ (show . labNodes $ graph) ++ "\n" ++ (show . labEdges $ graph) ++ "\n"
               return graph
             else do
               putStrLn $ "Reading graph from \"" ++ (head args) ++ "\"."
               [line1, line2] <- lines <$> (readFile $ head args)
               return $ mkGraph (read line1) (read line2)
  
  putStrLn $ "Writing graph in dot format to \"" ++ outputDotFilename ++ "\"."
  writeFile outputDotFilename $ graphviz graph "Etage" (11.69, 8.27) (1, 1) Portrait
  
  let graphSize = noNodes graph
  sourceNode <- randomRIO (1, graphSize)
  
  putStrLn $ "Graph contains " ++ (show graphSize) ++ " nodes. Searching for shortest paths from node " ++ (show sourceNode) ++ "."
  
  incubate $ do
    nerveTest <- (growNeuron :: NerveOnlyFor (TestNeuron String Double)) (\o -> o { sourceNode, graph })
    nervePaths <- shortestPaths sourceNode graph
    
    nervePaths `attachTo` [TranslatableFor nerveTest]
