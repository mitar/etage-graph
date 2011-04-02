{-# LANGUAGE TypeFamilies, DeriveDataTypeable, NamedFieldPuns, BangPatterns #-}

module Main (
  main
) where

import Control.Applicative
import Control.Monad
import Control.DeepSeq
import Control.Exception
import Data.Data
import Data.Graph.Etage
import Data.Graph.Inductive hiding (nodes, edges, defaultGraphSize)
import Data.List
import qualified Data.Map as M
import Data.Map hiding (filter, map, empty, null, lookup, insert)
import Data.Maybe
import Data.Time.Clock.POSIX
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Random
import System.IO
import Text.Printf

import Control.Etage

defaultGraphSize :: Int
defaultGraphSize = 6

data Option = InputGraph String | OutputGraph String | OutputDot String | SearchNode Node | GraphSize Int | Help deriving (Eq, Show)

options :: [OptDescr Option]
options = [
    Option ['g'] ["graph"]  (ReqArg InputGraph "filename")             "read graph grom a file, default is to generate one randomly",
    Option ['o'] ["output"] (ReqArg OutputGraph "filename")            "save graph to a file",
    Option ['d'] ["dot"]    (ReqArg OutputDot "filename")              "save graph to a file in a GraphViz format",
    Option ['n'] ["node"]   (ReqArg (SearchNode . readParam) "number") "index of the node to search shortest paths from, default is to pick one randomly",
    Option ['s'] ["size"]   (ReqArg (GraphSize . readParam) "number")  ("size of the randomly generated graph, default is " ++ (show defaultGraphSize)),
    Option ['h'] ["help"]   (NoArg Help)                               "show this help"
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

isSearchNode :: Option -> Bool
isSearchNode (SearchNode _) = True
isSearchNode _              = False

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

instance (Show a, Data a, Show b, Data b, Real b, Bounded b, NFData b) => Neuron (TestNeuron a b) where
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
  
  live nerve (TestNeuron sourceNode graph) = do
      before <- getPOSIXTime
      let pathsLazy = fromList . map (\(LP (n@(node, len):nodes)) -> (node, (len, reverse . map fst $ n:nodes))) $ spTree sourceNode graph
          !paths = pathsLazy `deepseq` pathsLazy
      after <- getPOSIXTime
      putStrLn $ "Standard search time: " ++ (show $ after - before)
      before' <- getPOSIXTime
      waitAndTest M.empty paths before'
    where waitAndTest currentPaths paths before = do
            impulse <- getForNeuron nerve
            case impulse of
              TopologyChange {}                                                 -> waitAndTest currentPaths paths before
              AddOutEdges {}                                                    -> waitAndTest currentPaths paths before
              TopologyUpdate { destination = (node, _), path = (LP path, len) } -> do
                let currentPaths' = M.insert node (len, map fst path) currentPaths
                    found = (fromIntegral $ size currentPaths) / (fromIntegral $ size paths) :: Float
                    found' = (fromIntegral $ size currentPaths') / (fromIntegral $ size paths) :: Float
                    shortest = (fromIntegral . sum . map fromEnum . elems $ intersectionWith (\(l, p) (l', p') -> l == l' && p == p') paths currentPaths') / (fromIntegral $ size paths) :: Float
                putStrLn $ "Found " ++ (printf "%.2f %%" (found' * 100)) ++ " paths and of those " ++ (printf "%.2f %%" (shortest * 100)) ++ " shortest."
                when (found' == 1.0 && found' > found) $ do
                  after <- getPOSIXTime
                  putStrLn $ "Etage search time for all paths: " ++ (show $ after - before)
                when (shortest == 1.0) $ do
                  after <- getPOSIXTime
                  putStrLn $ "Etage search time for shortest paths: " ++ (show $ after - before)
                  dissolving currentPaths'
                waitAndTest currentPaths' paths before

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
                            [line1, line2] <- lines <$> (readFile inputGraph)
                            let g = mkGraph (read line1) (read line2)
                            forceStrictGraph g
                            return (g, noNodes g)
                          _                            -> do
                            let GraphSize s = fromMaybe (GraphSize defaultGraphSize) $ find isGraphSize opts
                            putStrLn $ "Generating a random graph of size " ++ (show s) ++ "."
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
  
  sourceNode <- case find isSearchNode opts of
                  Just (SearchNode s) -> do
                    when (s < 1 || s > graphSize) $ throwIO $ ErrorCall $ "invalid parameter `" ++ (show s) ++ "'"
                    return s
                  _                   -> randomRIO (1, graphSize)
  
  putStrLn $ "Graph contains " ++ (show graphSize) ++ " nodes. Searching for shortest paths from node " ++ (show sourceNode) ++ "."
  
  incubate $ do
    nerveTest <- (growNeuron :: NerveOnlyFor (TestNeuron String Double)) (\o -> o { sourceNode, graph })
    pathsNerves <- shortestPaths graph
    
    (pathsNerves ! sourceNode) `attachTo` [TranslatableFor nerveTest]
    
    sendTopologyChange pathsNerves

forceStrictGraph :: (NFData a, NFData b, Graph gr) => gr a b -> IO ()
forceStrictGraph g = labNodes g `deepseq` labEdges g `deepseq` return ()
