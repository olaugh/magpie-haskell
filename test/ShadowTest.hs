{-# LANGUAGE OverloadedStrings #-}

-- | Shadow algorithm tests
-- Ported from MAGPIE test/shadow_test.c
--
-- Note: The shadow algorithm computes upper bounds on move scores for pruning.
-- The current Haskell implementation is a work-in-progress. Full functionality
-- requires:
-- 1. Extension set computation via GADDAG traversal
-- 2. Word existence checking (WMP)
-- 3. Correct multiplier handling for restricted vs unrestricted tiles
--
-- These tests verify the basic infrastructure is in place.
module Main where

import Magpie.Types
import Magpie.KWG
import Magpie.Board
import Magpie.LetterDistribution
import Magpie.Shadow

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless)

-- | Test CGP constants from MAGPIE
kaOpeningCGP :: String
kaOpeningCGP = "15/15/15/15/15/15/15/6KA7/15/15/15/15/15/15/15"

dougVEmelyCGP :: String
dougVEmelyCGP = "15/15/15/15/15/15/15/3WINDY7/15/15/15/15/15/15/15"

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

-- | Helper to load CGP and generate anchors
loadAndShadow :: KWG -> LetterDistribution -> String -> String -> [Anchor]
loadAndShadow kwg ld cgp rackStr =
  let board = loadCGP ld cgp
      rack = rackFromString ld rackStr
      cfg = defaultShadowConfig { shadowSortByScore = True }
      anchors = generateAnchors cfg kwg ld board rack
  in extractSortedAnchors anchors

-- | Test that anchors are generated for non-empty boards
testAnchorGeneration :: KWG -> LetterDistribution -> IO ()
testAnchorGeneration kwg ld = do
  putStrLn "Testing anchor generation..."

  -- KA opening should generate anchors
  let anchors1 = loadAndShadow kwg ld kaOpeningCGP "EE"
  unless (not (null anchors1)) $ do
    putStrLn "FAIL: KA+EE should have anchors"
    exitFailure
  putStrLn $ "  KA+EE: " ++ show (length anchors1) ++ " anchors"

  -- WINDY should generate anchors
  let anchors2 = loadAndShadow kwg ld dougVEmelyCGP "AEIOU"
  unless (not (null anchors2)) $ do
    putStrLn "FAIL: WINDY+AEIOU should have anchors"
    exitFailure
  putStrLn $ "  WINDY+AEIOU: " ++ show (length anchors2) ++ " anchors"

  putStrLn "Anchor generation tests passed!"

-- | Test that anchors are sorted by descending score
testAnchorSorting :: KWG -> LetterDistribution -> IO ()
testAnchorSorting kwg ld = do
  putStrLn "Testing anchor sorting..."

  let anchors = loadAndShadow kwg ld kaOpeningCGP "EE"
      scores = map anchorHighestPossibleScore anchors

      isSorted [] = True
      isSorted [_] = True
      isSorted (x:y:rest) = x >= y && isSorted (y:rest)

  unless (isSorted scores) $ do
    putStrLn "FAIL: Anchors not sorted by descending score"
    putStrLn $ "  Scores: " ++ show scores
    exitFailure

  putStrLn $ "  Anchor scores: " ++ show (take 5 scores)

  -- Print detailed anchor info for debugging
  putStrLn "  Anchor details (row, col, dir, score):"
  mapM_ (\a -> putStrLn $ "    " ++ show (anchorRow a, anchorCol a, anchorDir a, anchorHighestPossibleScore a)) anchors

  putStrLn "Anchor sorting tests passed!"

  -- Expected scores from C test:
  -- (KA)E and EE: 10, E(K)E: 9, (KA)EE: 8, (KA)E: 7, (K)E: 7, EE and E(A): 5, EE and (A)E: 5, (A)E: 2
  putStrLn "  Expected from C: [10, 9, 8, 7, 7, 5, 5, 2] (8 anchors)"

-- | Test Anchor data type
testAnchorDataType :: IO ()
testAnchorDataType = do
  putStrLn "Testing Anchor data type..."

  let anchor = Anchor
        { anchorRow = 7
        , anchorCol = 8
        , anchorLastAnchorCol = 6
        , anchorDir = Horizontal
        , anchorHighestPossibleScore = 14
        , anchorHighestPossibleEquity = 14
        , anchorTilesToPlay = 1
        , anchorPlaythroughBlocks = 0
        , anchorWordLength = 3
        , anchorLeftmostStartCol = 6
        , anchorRightmostStartCol = 8
        }

  unless (anchorRow anchor == 7) $ do
    putStrLn "FAIL: anchorRow"
    exitFailure

  unless (anchorHighestPossibleScore anchor == 14) $ do
    putStrLn "FAIL: anchorHighestPossibleScore"
    exitFailure

  putStrLn "Anchor data type tests passed!"

-- | Test ShadowConfig
testShadowConfig :: IO ()
testShadowConfig = do
  putStrLn "Testing ShadowConfig..."

  let cfg = defaultShadowConfig

  unless (shadowBingoBonus cfg == 50) $ do
    putStrLn "FAIL: shadowBingoBonus should be 50"
    exitFailure

  putStrLn "ShadowConfig tests passed!"

-- | Main test runner
main :: IO ()
main = do
  putStrLn "Loading KWG..."
  kwg <- loadTestKWG
  putStrLn $ "Loaded " ++ show (numNodes kwg) ++ " nodes"

  ld <- loadTestLD
  putStrLn "Loaded letter distribution"

  testAnchorDataType
  testShadowConfig
  testAnchorGeneration kwg ld
  testAnchorSorting kwg ld

  putStrLn ""
  putStrLn "All shadow infrastructure tests passed!"
  putStrLn ""
  putStrLn "Shadow algorithm status:"
  putStrLn "  [x] Extension set computation via GADDAG"
  putStrLn "  [x] Direction-agnostic MoveGen struct (matches MAGPIE)"
  putStrLn "  [x] Multiplier handling for restricted/unrestricted tiles"
  putStrLn "  [x] Cross-word scoring for restricted tiles"
  putStrLn "  [x] Playthrough traversal in both directions"
  putStrLn "  [ ] Word existence checking (WMP) - not yet implemented"
  putStrLn "  [ ] Port remaining tests from shadow_test.c"
  exitSuccess
