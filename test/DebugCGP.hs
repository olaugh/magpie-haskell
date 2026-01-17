module Main where

import Magpie.Board
import Magpie.LetterDistribution
import Magpie.KWG
import Magpie.Types

main :: IO ()
main = do
  ld <- loadLetterDistribution "data/letterdistributions/english.csv"
  kwg <- loadKWG "data/lexica/NWL20.kwg"

  -- Simple test first: row 4 with " A"
  putStrLn "=== Simple test: ' A' at row 4 ==="
  let board1 = setRow ld standardBoard 4 " A"
  putStrLn "Row 4 contents after setRow:"
  mapM_ (\c -> do
    let ml = getLetter board1 4 c
        ch = if unML ml == 0 then '.' else ldToChar ld ml
    putStrLn $ "  Col " ++ show c ++ ": " ++ [ch] ++ " (ml=" ++ show (unML ml) ++ ")"
    ) [0..4]

  putStrLn "\nComputing cross sets with Vertical direction..."
  let boardV1 = computeCrossSets kwg ld Vertical board1
  putStrLn $ "Cross set at (4,0): " ++ show (getCrossSet boardV1 4 0)
  putStrLn $ "Cross score at (4,0): " ++ show (getCrossScore boardV1 4 0)
  putStrLn "Expected: cross set for 2-letter words ending in A (AA, BA, DA, etc.)"

  -- Test KWG lookup directly
  putStrLn "\n=== Direct KWG test ==="
  putStrLn $ "KWG has " ++ show (numNodes kwg) ++ " nodes"
  let root = dawgRoot kwg
  putStrLn $ "DAWG root: " ++ show root

  -- Test: Is 'AA' a word?
  putStrLn "\nTesting word 'AA':"
  let aNode = getNextNodeIndex kwg root (MachineLetter 1)  -- A = 1
  putStrLn $ "  After A: node = " ++ show aNode
  let aaNode = getNextNodeIndex kwg aNode (MachineLetter 1)  -- A = 1
  putStrLn $ "  After AA: node = " ++ show aaNode
  let aaNode' = getNode kwg aaNode
  putStrLn $ "  Node accepts (is end): " ++ show (nodeIsEnd aaNode')
