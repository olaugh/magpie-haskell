{-# LANGUAGE OverloadedStrings #-}

-- | Test that shadow-pruned move generation finds the same best move as full generation
module Main where

import Magpie.Types
import Magpie.KWG
import Magpie.Board
import Magpie.LetterDistribution
import Magpie.MoveGen

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless, forM_)

-- | Data paths
dataPath :: String
dataPath = "data/"

-- | Load KWG from standard location
loadTestKWG :: IO KWG
loadTestKWG = loadKWG (dataPath ++ "lexica/CSW21.kwg")

-- | Load default letter distribution
loadTestLD :: IO LetterDistribution
loadTestLD = return defaultEnglishLD

-- | Helper to create a rack from a string
rackFromString :: LetterDistribution -> String -> Rack
rackFromString ld str =
  case ldFromString ld str of
    Nothing -> error $ "Invalid rack string: " ++ str
    Just mls -> rackFromList (ldSize ld) mls

-- | Test cases: (CGP, rack) pairs
testCases :: [(String, String)]
testCases =
  [ ("15/15/15/15/15/15/15/15/15/15/15/15/15/15/15", "AEIOU")  -- Empty board
  , ("15/15/15/15/15/15/15/6KA7/15/15/15/15/15/15/15", "EE")   -- KA opening
  , ("15/15/15/15/15/15/15/6KA7/15/15/15/15/15/15/15", "TESTING")
  , ("15/15/15/15/15/15/15/3WINDY7/15/15/15/15/15/15/15", "AEIOU")
  , ("15/15/15/15/15/15/15/3WINDY7/15/15/15/15/15/15/15", "QXZJKBM")
  ]

-- | Run a single test case
runTestCase :: KWG -> LetterDistribution -> (String, String) -> IO Bool
runTestCase kwg ld (cgp, rackStr) = do
  let board = loadCGP ld cgp
      rack = rackFromString ld rackStr

      -- Generate all moves and get the best
      allMoves = generateMoves kwg ld board rack
      fullBest = case allMoves of
                   (m:_) -> m
                   [] -> Move Pass 0 0 Horizontal [] 0 0 (Equity 0)

      -- Generate best move with shadow pruning (no KLV/WMP, so compare by score)
      -- Use 100 for bagCount (full bag, exchanges allowed)
      shadowBest = generateBestMove defaultMoveGenConfig Nothing Nothing kwg ld board rack 100

      -- Compare scores
      fullScore = moveScore fullBest
      shadowScore = moveScore shadowBest

  if fullScore == shadowScore
    then do
      putStrLn $ "  PASS: " ++ rackStr ++ " -> score " ++ show fullScore
      return True
    else do
      putStrLn $ "  FAIL: " ++ rackStr
      putStrLn $ "    Full best:   " ++ show fullScore ++ " at (" ++ show (moveRow fullBest) ++ "," ++ show (moveCol fullBest) ++ ")"
      putStrLn $ "    Shadow best: " ++ show shadowScore ++ " at (" ++ show (moveRow shadowBest) ++ "," ++ show (moveCol shadowBest) ++ ")"
      return False

-- | Main test runner
main :: IO ()
main = do
  putStrLn "Loading KWG..."
  kwg <- loadTestKWG
  putStrLn $ "Loaded " ++ show (numNodes kwg) ++ " nodes"

  ld <- loadTestLD
  putStrLn "Loaded letter distribution"
  putStrLn ""

  putStrLn "Testing shadow-pruned move generation..."
  results <- mapM (runTestCase kwg ld) testCases

  let passed = length (filter id results)
      total = length results

  putStrLn ""
  putStrLn $ "Results: " ++ show passed ++ "/" ++ show total ++ " passed"

  if all id results
    then do
      putStrLn "All tests passed!"
      exitSuccess
    else do
      putStrLn "Some tests failed!"
      exitFailure
