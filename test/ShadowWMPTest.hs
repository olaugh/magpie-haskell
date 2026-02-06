{-# LANGUAGE OverloadedStrings #-}

-- | WMP Shadow tests ported from C MAGPIE
-- These test that WMP shadow scoring produces the expected bounds
module Main where

import Magpie
import Magpie.Shadow
import Magpie.WMPMoveGen
import Magpie.WMP (loadWMP, WMP)
import Magpie.KLV (loadKLV, KLV)
import Magpie.MoveGen (generateBestMove, defaultMoveGenConfig)

import System.Exit (exitFailure, exitSuccess)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Control.Monad (when, forM_)
import Text.Printf (printf)

-- | Test configuration
data TestConfig = TestConfig
  { tcKWG :: KWG
  , tcLD  :: LetterDistribution
  , tcWMP :: Maybe WMP
  , tcKLV :: Maybe KLV
  }

-- | Load test configuration
loadTestConfig :: IO TestConfig
loadTestConfig = do
  kwg <- loadKWG "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/lexica/CSW21.kwg"
  let ld = defaultEnglishLD
  wmp <- Just <$> loadWMP "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/lexica/CSW21.wmp"
  klv <- Just <$> loadKLV "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/lexica/CSW21.klv2"
  return $ TestConfig kwg ld wmp klv

-- | Generate shadow anchors for a rack on an empty board (multi-anchor mode)
generateShadowAnchorsMulti :: TestConfig -> String -> [Anchor]
generateShadowAnchorsMulti tc rackStr =
  case ldFromString (tcLD tc) rackStr of
    Nothing -> []
    Just mls ->
      let rack = rackFromList (ldSize (tcLD tc)) mls
          board = standardBoard
          wmg = case tcWMP tc of
                  Just wmp -> wmpMoveGenCheckNonplaythroughExistence False (const (Equity 0)) $
                              wmpMoveGenResetPlaythrough $
                              wmpMoveGenInit (Just wmp) rack
                  Nothing -> wmpMoveGenInit Nothing rack
          -- Multi-anchor mode for testing WMP per-length anchors
          cfg = defaultShadowConfig { shadowBingoBonus = 50, shadowBagCount = 100, shadowMultiAnchor = True }
      in generateAnchors cfg (tcKWG tc) (tcLD tc) board rack wmg

-- | Generate shadow anchors for a rack on an empty board (single-anchor mode, default)
generateShadowAnchors :: TestConfig -> String -> [Anchor]
generateShadowAnchors tc rackStr =
  case ldFromString (tcLD tc) rackStr of
    Nothing -> []
    Just mls ->
      let rack = rackFromList (ldSize (tcLD tc)) mls
          board = standardBoard
          wmg = case tcWMP tc of
                  Just wmp -> wmpMoveGenCheckNonplaythroughExistence False (const (Equity 0)) $
                              wmpMoveGenResetPlaythrough $
                              wmpMoveGenInit (Just wmp) rack
                  Nothing -> wmpMoveGenInit Nothing rack
          cfg = defaultShadowConfig { shadowBingoBonus = 50, shadowBagCount = 100 }
      in generateAnchors cfg (tcKWG tc) (tcLD tc) board rack wmg

-- | Assert anchor count
assertAnchorCount :: String -> Int -> [Anchor] -> IO Bool
assertAnchorCount rackStr expected anchors = do
  let actual = length anchors
  if actual == expected
    then return True
    else do
      printf "FAIL: %s - expected %d anchors, got %d\n" rackStr expected actual
      return False

-- | Assert anchor score at index
assertAnchorScore :: String -> Int -> Int -> [Anchor] -> IO Bool
assertAnchorScore rackStr idx expected anchors = do
  if idx >= length anchors
    then do
      printf "FAIL: %s - anchor index %d out of range (only %d anchors)\n" rackStr idx (length anchors)
      return False
    else do
      let actual = anchorHighestPossibleScore (anchors !! idx)
      if actual == expected
        then return True
        else do
          printf "FAIL: %s[%d] - expected score %d, got %d\n" rackStr idx expected actual
          return False

-- | Assert anchor tiles to play at index
assertAnchorTilesToPlay :: String -> Int -> Int -> [Anchor] -> IO Bool
assertAnchorTilesToPlay rackStr idx expected anchors = do
  if idx >= length anchors
    then do
      printf "FAIL: %s - anchor index %d out of range\n" rackStr idx
      return False
    else do
      let actual = anchorTilesToPlay (anchors !! idx)
      if actual == expected
        then return True
        else do
          printf "FAIL: %s[%d] - expected tiles_to_play %d, got %d\n" rackStr idx expected actual
          return False

-- | Run WMP nonplaythrough existence tests
-- Ported from test_shadow_wmp_nonplaythrough_existence in shadow_test.c
-- These tests use multi-anchor mode to test per-word-length anchors
testWMPNonplaythroughExistence :: TestConfig -> IO Bool
testWMPNonplaythroughExistence tc = do
  putStrLn "\n=== Testing WMP Nonplaythrough Existence ==="

  -- Test MUZJIKS: No 1-tile anchor. 6 anchors for lengths 2-7.
  let anchors1 = generateShadowAnchorsMulti tc "MUZJIKS"
  r1 <- assertAnchorCount "MUZJIKS" 6 anchors1
  r2 <- assertAnchorScore "MUZJIKS" 0 128 anchors1  -- 7 tiles: MUZJIKS
  r3 <- assertAnchorTilesToPlay "MUZJIKS" 0 7 anchors1
  r4 <- assertAnchorScore "MUZJIKS" 1 76 anchors1   -- 6 tiles
  r5 <- assertAnchorTilesToPlay "MUZJIKS" 1 6 anchors1
  r6 <- assertAnchorScore "MUZJIKS" 2 74 anchors1   -- 5 tiles
  r7 <- assertAnchorTilesToPlay "MUZJIKS" 2 5 anchors1
  r8 <- assertAnchorScore "MUZJIKS" 3 52 anchors1   -- 4 tiles
  r9 <- assertAnchorScore "MUZJIKS" 4 46 anchors1   -- 3 tiles
  r10 <- assertAnchorScore "MUZJIKS" 5 36 anchors1  -- 2 tiles

  -- Test TRONGLE: No sevens (no bingo). 5 anchors for lengths 2-6.
  let anchors2 = generateShadowAnchorsMulti tc "TRONGLE"
  r11 <- assertAnchorCount "TRONGLE" 5 anchors2
  r12 <- assertAnchorScore "TRONGLE" 0 18 anchors2  -- 6 tiles
  r13 <- assertAnchorTilesToPlay "TRONGLE" 0 6 anchors2
  r14 <- assertAnchorScore "TRONGLE" 1 16 anchors2  -- 5 tiles
  r15 <- assertAnchorScore "TRONGLE" 2 10 anchors2  -- 4 tiles
  r16 <- assertAnchorScore "TRONGLE" 3 8 anchors2   -- 3 tiles
  r17 <- assertAnchorScore "TRONGLE" 4 6 anchors2   -- 2 tiles

  -- Test VVWWXYZ: No words at all. 0 anchors.
  let anchors3 = generateShadowAnchorsMulti tc "VVWWXYZ"
  r18 <- assertAnchorCount "VVWWXYZ" 0 anchors3

  let results = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18]
  let passed = length (filter id results)
  let total = length results
  printf "WMP Nonplaythrough Existence: %d/%d tests passed\n" passed total
  return (passed == total)

-- | Basic shadow score tests (without WMP)
testBasicShadowScore :: TestConfig -> IO Bool
testBasicShadowScore tc = do
  putStrLn "\n=== Testing Basic Shadow Scores ==="

  -- OU on empty board should have max score 4 (OU is a word)
  let anchors1 = generateShadowAnchors tc "OU"
  r1 <- assertAnchorCount "OU" 1 anchors1
  r2 <- assertAnchorScore "OU" 0 4 anchors1

  -- ID on empty board
  let anchors2 = generateShadowAnchors tc "ID"
  r3 <- assertAnchorCount "ID" 1 anchors2
  r4 <- assertAnchorScore "ID" 0 6 anchors2

  -- AX on empty board
  let anchors3 = generateShadowAnchors tc "AX"
  r5 <- assertAnchorCount "AX" 1 anchors3
  r6 <- assertAnchorScore "AX" 0 18 anchors3

  -- BD - no 2-letter words with B and D
  let anchors4 = generateShadowAnchors tc "BD"
  r7 <- assertAnchorCount "BD" 0 anchors4

  -- QK - no 2-letter words with Q and K
  let anchors5 = generateShadowAnchors tc "QK"
  r8 <- assertAnchorCount "QK" 0 anchors5

  let results = [r1, r2, r3, r4, r5, r6, r7, r8]
  let passed = length (filter id results)
  let total = length results
  printf "Basic Shadow Scores: %d/%d tests passed\n" passed total
  return (passed == total)

-- | Test that WMP and non-WMP modes find the same best move
-- This ensures the WMP optimization doesn't change results, just speeds things up
testWMPvsNonWMPComparison :: TestConfig -> IO Bool
testWMPvsNonWMPComparison tc = do
  putStrLn "\n=== Testing WMP vs Non-WMP Comparison ==="

  let testCases =
        [ ("AEIOU", standardBoard)                                         -- Empty board, common rack
        , ("MUZJIKS", standardBoard)                                       -- Bingo rack
        , ("TRONGLE", standardBoard)                                       -- No bingo rack
        , ("QXZJK", standardBoard)                                         -- Difficult rack
        , ("RETINAS", standardBoard)                                       -- Common bingo
        , ("EE", loadCGP (tcLD tc) "15/15/15/15/15/15/15/6KA7/15/15/15/15/15/15/15")
        , ("TESTING", loadCGP (tcLD tc) "15/15/15/15/15/15/15/6KA7/15/15/15/15/15/15/15")
        ]

  results <- mapM (runComparisonTest tc) testCases

  let passed = length (filter id results)
      total = length results
  printf "WMP vs Non-WMP Comparison: %d/%d tests passed\n" passed total
  return (passed == total)

-- | Run a single comparison test
runComparisonTest :: TestConfig -> (String, Board) -> IO Bool
runComparisonTest tc (rackStr, board) = do
  case ldFromString (tcLD tc) rackStr of
    Nothing -> do
      printf "SKIP: Invalid rack string: %s\n" rackStr
      return True  -- Skip invalid racks
    Just mls -> do
      let rack = rackFromList (ldSize (tcLD tc)) mls
          bagCount = 100  -- Full bag

          -- Generate best move WITHOUT WMP
          moveNoWMP = generateBestMove defaultMoveGenConfig (tcKLV tc) Nothing
                                       (tcKWG tc) (tcLD tc) board rack bagCount
          scoreNoWMP = moveScore moveNoWMP

          -- Generate best move WITH WMP
          moveWMP = generateBestMove defaultMoveGenConfig (tcKLV tc) (tcWMP tc)
                                     (tcKWG tc) (tcLD tc) board rack bagCount
          scoreWMP = moveScore moveWMP

      if scoreWMP == scoreNoWMP
        then do
          printf "  PASS: %s -> score %d\n" rackStr scoreWMP
          return True
        else do
          printf "  FAIL: %s\n" rackStr
          printf "    Non-WMP: score %d at (%d,%d)\n" scoreNoWMP (moveRow moveNoWMP) (moveCol moveNoWMP)
          printf "    WMP:     score %d at (%d,%d)\n" scoreWMP (moveRow moveWMP) (moveCol moveWMP)
          return False

-- | Compare anchor counts with and without WMP
testAnchorCounts :: TestConfig -> IO ()
testAnchorCounts tc = do
  putStrLn "\n=== Anchor Count Comparison ==="

  let testRack rackStr = do
        case ldFromString (tcLD tc) rackStr of
          Nothing -> return ()
          Just mls -> do
            let rack = rackFromList (ldSize (tcLD tc)) mls
                board = standardBoard
                cfg = defaultShadowConfig { shadowBingoBonus = 50, shadowBagCount = 100 }

                -- Without WMP
                wmgNoWmp = wmpMoveGenInit Nothing rack
                anchorsNoWmp = generateAnchors cfg (tcKWG tc) (tcLD tc) board rack wmgNoWmp

                -- With WMP
                wmgWmp = case tcWMP tc of
                  Just wmp -> wmpMoveGenCheckNonplaythroughExistence False (const (Equity 0)) $
                              wmpMoveGenResetPlaythrough $
                              wmpMoveGenInit (Just wmp) rack
                  Nothing -> wmgNoWmp
                anchorsWmp = generateAnchors cfg (tcKWG tc) (tcLD tc) board rack wmgWmp

            printf "  %s: No WMP=%d, WMP=%d (%.1fx)\n" rackStr
                   (length anchorsNoWmp) (length anchorsWmp)
                   (fromIntegral (length anchorsWmp) / fromIntegral (max 1 (length anchorsNoWmp)) :: Double)

  testRack "AEIOU"
  testRack "RETINAS"
  testRack "MUZJIKS"
  testRack "TRONGLE"

main :: IO ()
main = do
  putStrLn "Loading test configuration..."
  tc <- loadTestConfig
  putStrLn "Running WMP Shadow Tests...\n"

  r1 <- testBasicShadowScore tc
  r2 <- testWMPNonplaythroughExistence tc
  r3 <- testWMPvsNonWMPComparison tc
  testAnchorCounts tc

  putStrLn "\n=== Summary ==="
  if r1 && r2 && r3
    then do
      putStrLn "All tests passed!"
      exitSuccess
    else do
      putStrLn "Some tests failed!"
      exitFailure
