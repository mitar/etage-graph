module Main (
  main
) where

import Control.Monad
import Control.Exception
import Data.List
import System.Random

import Control.Etage
import Control.Etage.Dump
import Data.Graph.Etage
import Data.Graph.Inductive hiding (nodes, edges)

outputFilename :: String
outputFilename = "out.dot"

generateGraph :: Int -> IO (Gr Char Double)
generateGraph size = do
  when (size < 1 || size > 26) $ throwIO $ AssertionFailed $ "Graph size out of bounds " ++ show size
  let nodes = zip [1..size] ['A'..]
  edges <- fmap concat $ forM [1..size] $ \node -> do
    nedges <- randomRIO (0, size)
    others <- fmap (filter (node /=) . nub) $ forM [1..nedges] $ \_ -> randomRIO (1, size)
    gen <- getStdGen
    let weights = randomRs (1, 10) gen
    return $ zip3 (repeat node) others weights
  return $ mkGraph nodes edges

main :: IO ()
main = do
  graph <- generateGraph 6
  putStrLn $ "Writing graph in dot format to \"" ++ outputFilename ++ "\"."
  writeFile outputFilename $ graphviz graph "Etage" (8.5, 11) (1, 1) Portrait
  incubate $ do
    nervePaths <- shortestPaths 1 graph
    nerveDump <- (growNeuron :: NerveOnlyFor DumpNeuron) (\o -> o { showInsteadOfDump = True })
    
    nervePaths `attachTo` [TranslatableFor nerveDump]
