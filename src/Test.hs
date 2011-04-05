{-# LANGUAGE TypeFamilies, ScopedTypeVariables, DeriveDataTypeable, NamedFieldPuns, BangPatterns #-}

module Main (
  main
) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Control.Parallel.Strategies
import Data.Array hiding (elems)
import Data.Array.ST
import Data.Data
import Data.Graph.Etage
import Data.Graph.Inductive hiding (edges, defaultGraphSize)
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Time.Clock.POSIX
import GHC.Arr
import GHC.Conc
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random
import System.Timeout
import Text.Printf

import Control.Etage

defaultGraphSize :: Int
defaultGraphSize = 6

minCollectTimeout :: Int
minCollectTimeout = 500000 -- microseconds

initialCollectTimeout :: Int
initialCollectTimeout = 5000000 -- microseconds

data Option = InputGraph String | OutputGraph String | OutputDot String | GraphSize Int | Help deriving (Eq, Show)

options :: [OptDescr Option]
options = [
    Option "g" ["graph"]  (ReqArg InputGraph "filename")            "read graph grom a file, default is to generate one randomly",
    Option "o" ["output"] (ReqArg OutputGraph "filename")           "save graph to a file",
    Option "d" ["dot"]    (ReqArg OutputDot "filename")             "save graph to a file in a GraphViz format",
    Option "s" ["size"]   (ReqArg (GraphSize . readParam) "number") ("size of the randomly generated graph, default is " ++ show defaultGraphSize),
    Option "h" ["help"]   (NoArg Help)                              "show this help"
  ]
    where readParam param = case reads param of
                              [(p,[])] -> if p < 1
                                            then error $ "invalid parameter `" ++ param ++ "'"
                                            else p
                              _        -> error $ "invalid parameter `" ++ param ++ "'"

isInputGraph :: Option -> Bool
isInputGraph (InputGraph _) = True
isInputGraph _              = False

isGraphSize :: Option -> Bool
isGraphSize (GraphSize _) = True
isGraphSize _             = False

isOutputGraph :: Option -> Bool
isOutputGraph (OutputGraph _) = True
isOutputGraph _               = False

isOutputDot :: Option -> Bool
isOutputDot (OutputDot _) = True
isOutputDot _             = False

generateGraph :: Int -> IO (Gr String Double)
generateGraph graphSize = do
  when (graphSize < 1) $ throwIO $ AssertionFailed $ "Graph size out of bounds " ++ show graphSize
  let ns = map (\n -> (n, show n)) [1..graphSize]
  edges <- fmap concat $ forM [1..graphSize] $ \node -> do
    nedges <- randomRIO (0, graphSize)
    others <- fmap (filter (node /=) . nub) $ forM [1..nedges] $ \_ -> randomRIO (1, graphSize)
    gen <- getStdGen
    let weights = randomRs (1, 10) gen
    return $ zip3 (repeat node) others weights
  return $ mkGraph ns edges

data TestNeuron a b = TestNeuron Int (Array (Node, Node) (b, [Node])) deriving (Typeable)

instance (Show a, Data a, Show b, Data b, Real b, Bounded b, NFData b) => Neuron (TestNeuron a b) where
  type NeuronFromImpulse (TestNeuron a b) = NoImpulse
  type NeuronForImpulse (TestNeuron a b) = GraphImpulse a b
  data NeuronOptions (TestNeuron a b) = NodeOptions {
      graphSize :: Int,
      knownPaths :: Array (Node, Node) (b, [Node])
    }

  mkDefaultOptions = return NodeOptions {
      graphSize = undefined,
      knownPaths = undefined
    }

  grow NodeOptions { graphSize, knownPaths } = return $ TestNeuron graphSize knownPaths
  
  live nerve (TestNeuron graphSize knownPaths) = do
    before <- getPOSIXTime
    pathsLazy <- stToIO $ newArray ((1, 1), (graphSize, graphSize)) (maxBound, [])
    collectTimeout <- collectPaths initialCollectTimeout pathsLazy
    pathsLazy' <- stToIO $ unsafeFreezeSTArray pathsLazy
    let !paths = pathsLazy' `using` evalTraversable rdeepseq
    after <- getPOSIXTime
    putStrLn $ "Etage search time for shortest paths: " ++ show (after - before - fromRational (fromIntegral collectTimeout % 1000000)) ++ " (" ++ printf "%fs" ((fromIntegral collectTimeout :: Double) / 1000000) ++ " timeout)" -- we correct for the last timeout
    let paths'      = M.fromList $ assocs paths
        knownPaths' = M.fromList $ assocs knownPaths
        shortest    = (fromIntegral . sum . map fromEnum . M.elems $ M.intersectionWith (\(l, p) (l', p') -> l == l' && p == p') knownPaths' paths') / fromIntegral (M.size knownPaths') :: Float
    putStrLn $ "Found " ++ printf "%.2f %%" (shortest * 100) ++ " shortest paths."
    dissolving ()
      where collectPaths :: Int -> STArray RealWorld (Node, Node) (b, [Node]) -> IO Int
            collectPaths collectTimeout arr = do
              before <- getPOSIXTime
              impulse <- timeout collectTimeout $ getForNeuron nerve
              case impulse of
                Nothing -> return collectTimeout
                Just i  -> do
                  let timestamp       = impulseTimestamp i
                      -- TODO: Improve timeout handling. Timeout only after the first TopologyChange?
                      collectTimeout' = max ((collectTimeout + round ((timestamp - before) * 2 * 1000000)) `div` 2) minCollectTimeout
                  case i of
                    TopologyChange {}                                                 -> collectPaths collectTimeout' arr
                    AddOutEdges {}                                                    -> collectPaths collectTimeout' arr
                    TopologyUpdate { destination = (node, _), path = (LP path, len) } -> do
                      let sourceNode = fst . head $ path
                      stToIO $ writeArray arr (sourceNode, node) (len, map fst path)
                      collectPaths collectTimeout' arr

main :: IO ()
main = do
  prepareEnvironment
  
  args <- getArgs
  opts <- case getOpt Permute options args of
            (o, [], [])  -> return o
            (_, p:_, []) -> throwIO $ ErrorCall $ "unrecognized option `" ++ p ++ "'"
            (_, _, errs) -> throwIO $ ErrorCall $ head $ lines $ head errs

  when (Help `elem` opts) $ do
    programName <- getProgName
    putStrLn $ "Usage:\n" ++ usageInfo (programName ++ " [option ...]" ++ "\n\nOptions:") options
    exitWith ExitSuccess
  
  (graph, graphSize) <- case find isInputGraph opts of
                          Just (InputGraph inputGraph) -> do
                            when (any isGraphSize opts) $ throwIO $ ErrorCall $ "conflicting options `" ++ "--graph" ++ "' and `" ++ "--size" ++ "'"
                            putStrLn $ "Reading graph from \"" ++ inputGraph ++ "\"."
                            [line1, line2] <- lines <$> readFile inputGraph
                            let g = mkGraph (read line1) (read line2)
                            forceStrictGraph g
                            return (g, noNodes g)
                          _                            -> do
                            let GraphSize s = fromMaybe (GraphSize defaultGraphSize) $ find isGraphSize opts
                            putStrLn $ "Generating a random graph of size " ++ show s ++ "."
                            g <- generateGraph s
                            forceStrictGraph g
                            return (g, s)

  case find isOutputGraph opts of
    Just (OutputGraph outputGraph) -> do
      putStrLn $ "Writing graph to \"" ++ outputGraph ++ "\"."
      writeFile outputGraph $ (show . labNodes $ graph) ++ "\n" ++ (show . labEdges $ graph) ++ "\n"
    _                              -> return ()

  case find isOutputDot opts of
    Just (OutputDot outputDot) -> do
      putStrLn $ "Writing graph in dot format to \"" ++ outputDot ++ "\"."
      writeFile outputDot $ graphviz graph "Etage" (8.27, 11.69) (1, 1) Landscape
    _                          -> return ()
  
  putStrLn $ "Graph contains " ++ show graphSize ++ " nodes."
  
  before <- getPOSIXTime
  let !paths = dijkstraShortestPaths graph graphSize `using` evalTraversable rdeepseq
  after <- getPOSIXTime
  putStrLn $ "Dijkstra search time for shortest paths: " ++ show (after - before)

  incubate $ do
    nerveTest <- (growNeuron :: NerveOnlyFor (TestNeuron String Double)) (\o -> o { graphSize, knownPaths = paths })
    pathsNerves <- shortestPaths graph
    
    mapM_ (`attachTo` [TranslatableFor nerveTest]) $ M.elems pathsNerves
    
    sendTopologyChange pathsNerves

forceStrictGraph :: (NFData a, NFData b, Graph gr) => gr a b -> IO ()
forceStrictGraph g = labNodes g `deepseq` labEdges g `deepseq` return ()

dijkstraShortestPaths :: (Graph gr, Bounded b, Real b, NFData b) => gr a b -> Int -> Array (Node, Node) (b, [Node])
dijkstraShortestPaths graph graphSize = array ((1, 1), (graphSize, graphSize)) $ initialValues ++ concat pathsValues
  where initialValues           = [ ((i, j), (maxBound, [])) | i <- [1..graphSize], j <- [1..graphSize] ]
        pathsValues             = map spFromSource (nodes graph) `using` parListChunk (noNodes graph `div` (numCapabilities * 10)) rdeepseq
        spFromSource sourceNode = map (\(LP (n@(node, len):ns)) -> ((sourceNode, node), (len, reverse . map fst $ n:ns))) $ spTree sourceNode graph
