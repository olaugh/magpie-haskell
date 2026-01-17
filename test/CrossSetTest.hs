{-# LANGUAGE OverloadedStrings #-}

-- | Cross set tests ported from MAGPIE
module Main where

import Magpie.Types
import Magpie.KWG
import Magpie.LetterDistribution
import Magpie.Board
import Magpie.MoveGen

import Data.Word (Word64)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)

-- | Test configuration
data TestConfig = TestConfig
  { tcKWG :: KWG
  , tcLD  :: LetterDistribution
  , tcName :: String
  }

-- | Load test configuration for a lexicon
loadTestConfig :: String -> IO TestConfig
loadTestConfig lexName = do
  let kwgPath = "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/lexica/" ++ lexName ++ ".kwg"
  kwg <- loadKWG kwgPath
  ld <- loadLetterDistribution "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/letterdistributions/english.csv"
  return $ TestConfig kwg ld lexName

-- | Trivial cross set string (all letters)
trivialCrossSetString :: String
trivialCrossSetString = "?ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | CGP board definitions from MAGPIE
vsMatt :: String
vsMatt = "7ZEP1F3/1FLUKY3R1R3/5EX2A1U3/2SCARIEST1I3/9TOT3/6GO1LO4/6OR1ETA3/6JABS1b3/5QI4A3/5I1N3N3/3ReSPOND1D3/1HOE3V3O3/1ENCOMIA3N3/7T7/3VENGED6"

vsEd :: String
vsEd = "14E/14N/14d/14U/4GLOWS5R/8PET3E/7FAXING1R/6JAY1TEEMS/2B2BOY4N2/2L1DOE5U2/2ANEW5PI2/2MO1LEU3ON2/2EH7HE2/15/15"

-- | Assert equality with message
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual
  | expected == actual = return ()
  | otherwise = do
      putStrLn $ "FAIL: " ++ msg
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Actual:   " ++ show actual
      exitFailure

-- | Test cross set at a specific position
-- In MAGPIE, BOARD_VERTICAL_DIRECTION cross set constrains letters when playing VERTICALLY
-- which means checking HORIZONTAL cross words (neighbors to left/right)
-- In our code, computeCrossSets Vertical checks horizontal neighbors (crossDir = Horizontal)
testGenCrossSet :: TestConfig -> Board -> Int -> Int -> String -> Int -> IO ()
testGenCrossSet tc board row col expectedLetters expectedScore = do
  -- Use Vertical direction to compute horizontal cross sets (matching MAGPIE's VERTICAL_DIRECTION)
  let boardWithCrossSets = computeCrossSets (tcKWG tc) (tcLD tc) Vertical board
      actualCrossSet = getCrossSet boardWithCrossSets row col
      actualCrossScore = getCrossScore boardWithCrossSets row col
      expectedCrossSet = stringToCrossSet (tcLD tc) expectedLetters

  assertEqual ("cross set at (" ++ show row ++ "," ++ show col ++ ")")
    expectedCrossSet actualCrossSet
  assertEqual ("cross score at (" ++ show row ++ "," ++ show col ++ ")")
    expectedScore actualCrossScore

-- | Test cross set after setting a row
-- Uses MAGPIE's VERTICAL_DIRECTION semantics (checks horizontal neighbors)
testGenCrossSetRow :: TestConfig -> Int -> Int -> String -> String -> Int -> IO ()
testGenCrossSetRow tc row col rowContent expectedLetters expectedScore = do
  let board = setRow (tcLD tc) standardBoard row rowContent
      -- Use Vertical direction to compute horizontal cross sets
      boardWithCrossSets = computeCrossSets (tcKWG tc) (tcLD tc) Vertical board
      actualCrossSet = getCrossSet boardWithCrossSets row col
      actualCrossScore = getCrossScore boardWithCrossSets row col
      expectedCrossSet = stringToCrossSet (tcLD tc) expectedLetters

  when (expectedCrossSet /= actualCrossSet || expectedScore /= actualCrossScore) $ do
    putStrLn $ "FAIL: row=" ++ show row ++ " col=" ++ show col ++ " content=" ++ show rowContent
    putStrLn $ "  Expected letters: " ++ show expectedLetters
    putStrLn $ "  Actual cross set: " ++ show actualCrossSet
    putStrLn $ "  Expected cross set: " ++ show expectedCrossSet
    putStrLn $ "  Expected score: " ++ show expectedScore
    putStrLn $ "  Actual score: " ++ show actualCrossScore
    exitFailure

--------------------------------------------------------------------------------
-- Tests from MAGPIE cross_set_test.c
--------------------------------------------------------------------------------

-- | Test cross sets on loaded VS_MATT game
testCrossSetLoadedGame :: TestConfig -> IO ()
testCrossSetLoadedGame tc = do
  putStrLn $ "  Testing " ++ tcName tc ++ " VS_MATT cross sets..."
  let board = loadCGP (tcLD tc) vsMatt
      -- Use Vertical to compute horizontal cross sets (MAGPIE's VERTICAL_DIRECTION)
      boardV = computeCrossSets (tcKWG tc) (tcLD tc) Vertical board

  -- MAGPIE's VERTICAL_DIRECTION cross sets (checks horizontal neighbors)
  assertEqual "VS_MATT (10,10) cross set"
    (stringToCrossSet (tcLD tc) "?E")
    (getCrossSet boardV 10 10)
  assertEqual "VS_MATT (10,10) cross score" 11 (getCrossScore boardV 10 10)

  assertEqual "VS_MATT (2,4) cross set"
    (stringToCrossSet (tcLD tc) "?DHKLRSV")
    (getCrossSet boardV 2 4)
  assertEqual "VS_MATT (2,4) cross score" 9 (getCrossScore boardV 2 4)

  assertEqual "VS_MATT (8,7) cross set"
    (stringToCrossSet (tcLD tc) "?S")
    (getCrossSet boardV 8 7)
  assertEqual "VS_MATT (8,7) cross score" 11 (getCrossScore boardV 8 7)

  assertEqual "VS_MATT (12,8) cross set - empty"
    (stringToCrossSet (tcLD tc) "")
    (getCrossSet boardV 12 8)
  assertEqual "VS_MATT (12,8) cross score" 11 (getCrossScore boardV 12 8)

  assertEqual "VS_MATT (3,1) cross set - empty"
    (stringToCrossSet (tcLD tc) "")
    (getCrossSet boardV 3 1)
  assertEqual "VS_MATT (3,1) cross score" 10 (getCrossScore boardV 3 1)

  assertEqual "VS_MATT (6,8) cross set - empty"
    (stringToCrossSet (tcLD tc) "")
    (getCrossSet boardV 6 8)
  assertEqual "VS_MATT (6,8) cross score" 5 (getCrossScore boardV 6 8)

  assertEqual "VS_MATT (2,10) cross set"
    (stringToCrossSet (tcLD tc) "?M")
    (getCrossSet boardV 2 10)
  assertEqual "VS_MATT (2,10) cross score" 2 (getCrossScore boardV 2 10)

  -- MAGPIE's HORIZONTAL_DIRECTION (transpose board, use Vertical = checks vertical neighbors)
  let boardT = transpose board
      boardTV = computeCrossSets (tcKWG tc) (tcLD tc) Vertical boardT

  assertEqual "VS_MATT transposed (10,10) cross set - trivial"
    (trivialCrossSet (ldSize (tcLD tc)))
    (getCrossSet boardTV 10 10)
  assertEqual "VS_MATT transposed (10,10) cross score" 0 (getCrossScore boardTV 10 10)

  assertEqual "VS_MATT transposed (2,4) cross set"
    (stringToCrossSet (tcLD tc) "?HIO")
    (getCrossSet boardTV 2 4)
  assertEqual "VS_MATT transposed (2,4) cross score" 1 (getCrossScore boardTV 2 4)

  assertEqual "VS_MATT transposed (12,8) cross set - trivial"
    (trivialCrossSet (ldSize (tcLD tc)))
    (getCrossSet boardTV 12 8)
  assertEqual "VS_MATT transposed (12,8) cross score" 0 (getCrossScore boardTV 12 8)

  assertEqual "VS_MATT transposed (2,10) cross set"
    (stringToCrossSet (tcLD tc) "?CDEFHIMSTWY")
    (getCrossSet boardTV 2 10)
  assertEqual "VS_MATT transposed (2,10) cross score" 2 (getCrossScore boardTV 2 10)

-- | Test cross sets at edges (set row tests)
testCrossSetEdges :: TestConfig -> IO ()
testCrossSetEdges tc = do
  putStrLn $ "  Testing " ++ tcName tc ++ " edge cross sets..."

  testGenCrossSetRow tc 4 0 " A" "?ABDFHKLMNPTYZ" 1
  testGenCrossSetRow tc 4 1 "A" "?ABDEGHILMNRSTWXY" 1
  testGenCrossSetRow tc 4 13 "              F" "?EIO" 4
  testGenCrossSetRow tc 4 14 "             F " "?AE" 4
  testGenCrossSetRow tc 4 14 "          WECH " "?T" 12
  testGenCrossSetRow tc 4 14 "           ZZZ " "" 30
  testGenCrossSetRow tc 4 14 "       ZYZZYVA " "?S" 43
  testGenCrossSetRow tc 4 14 "        ZYZZYV " "?A" 42
  testGenCrossSetRow tc 4 14 "       Z Z Y A " "?ABDEGHILMNRSTWXY" 1
  testGenCrossSetRow tc 4 12 "       z z Y A " "?E" 5
  testGenCrossSetRow tc 4 14 "OxYpHeNbUTAzON " "?E" 15
  testGenCrossSetRow tc 4 6 "OXYPHE BUTAZONE" "?N" 40
  testGenCrossSetRow tc 4 0 " YHJKTKHKTLV" "" 42
  testGenCrossSetRow tc 4 14 "   YHJKTKHKTLV " "" 42
  testGenCrossSetRow tc 4 6 "YHJKTK HKTLV" "" 42

-- | Test cross sets with tiles on either side
testCrossSetBetween :: TestConfig -> IO ()
testCrossSetBetween tc = do
  putStrLn $ "  Testing " ++ tcName tc ++ " between tiles cross sets..."

  testGenCrossSetRow tc 4 1 "D NATURES" "?E" 9
  testGenCrossSetRow tc 4 1 "D N" "?AEIOU" 3
  testGenCrossSetRow tc 4 1 "D NT" "?EIU" 4
  testGenCrossSetRow tc 4 1 "D NTS" "?EIU" 5
  testGenCrossSetRow tc 4 1 "R VOTED" "?E" 10
  testGenCrossSetRow tc 4 5 "PHENY BUTAZONE" "?L" 32
  testGenCrossSetRow tc 4 6 "OXYPHE BUTAZONE" "?N" 40
  testGenCrossSetRow tc 4 1 "R XED" "?A" 12
  testGenCrossSetRow tc 4 2 "BA ED" "?AKLNRSTY" 7
  testGenCrossSetRow tc 4 1 "X Z" "" 18
  testGenCrossSetRow tc 4 6 "STRONG L" "?Y" 8
  testGenCrossSetRow tc 4 1 "W SIWYG" "?Y" 16
  testGenCrossSetRow tc 4 0 " EMSTVO" "?Z" 11
  testGenCrossSetRow tc 4 1 "T UNFOLD" "" 11
  testGenCrossSetRow tc 4 1 "S OBCONIc" "" 11

-- | Test cross sets on VS_ED board
testCrossSetVsEd :: TestConfig -> IO ()
testCrossSetVsEd tc = do
  putStrLn $ "  Testing " ++ tcName tc ++ " VS_ED cross sets..."
  let board = loadCGP (tcLD tc) vsEd
      -- Use Vertical to compute horizontal cross sets (MAGPIE's VERTICAL_DIRECTION)
      boardV = computeCrossSets (tcKWG tc) (tcLD tc) Vertical board

  assertEqual "VS_ED (8,8) cross set"
    (stringToCrossSet (tcLD tc) "?OS")
    (getCrossSet boardV 8 8)
  assertEqual "VS_ED (8,8) cross score" 8 (getCrossScore boardV 8 8)

  -- Transpose and check (MAGPIE's HORIZONTAL_DIRECTION)
  let boardT = transpose board
      boardTV = computeCrossSets (tcKWG tc) (tcLD tc) Vertical boardT

  assertEqual "VS_ED transposed (8,8) cross set"
    (stringToCrossSet (tcLD tc) "?S")
    (getCrossSet boardTV 8 8)
  assertEqual "VS_ED transposed (8,8) cross score" 9 (getCrossScore boardTV 8 8)

  -- More positions (reuse boardV for non-transposed)
  assertEqual "VS_ED (5,11) cross set"
    (stringToCrossSet (tcLD tc) "?S")
    (getCrossSet boardV 5 11)
  assertEqual "VS_ED (5,11) cross score" 5 (getCrossScore boardV 5 11)

  assertEqual "VS_ED transposed (11,5) cross set"
    (stringToCrossSet (tcLD tc) "?AO")
    (getCrossSet boardTV 11 5)
  assertEqual "VS_ED transposed (11,5) cross score" 2 (getCrossScore boardTV 11 5)

  assertEqual "VS_ED (8,13) cross set"
    (stringToCrossSet (tcLD tc) "?AEOU")
    (getCrossSet boardV 8 13)
  assertEqual "VS_ED (8,13) cross score" 1 (getCrossScore boardV 8 13)

  assertEqual "VS_ED transposed (13,8) cross set"
    (stringToCrossSet (tcLD tc) "?AEIMOUY")
    (getCrossSet boardTV 13 8)
  assertEqual "VS_ED transposed (13,8) cross score" 3 (getCrossScore boardTV 13 8)

  assertEqual "VS_ED (9,13) cross set"
    (stringToCrossSet (tcLD tc) "?HMNPST")
    (getCrossSet boardV 9 13)
  assertEqual "VS_ED (9,13) cross score" 1 (getCrossScore boardV 9 13)

  assertEqual "VS_ED transposed (13,9) - trivial"
    (trivialCrossSet (ldSize (tcLD tc)))
    (getCrossSet boardTV 13 9)
  assertEqual "VS_ED transposed (13,9) cross score" 0 (getCrossScore boardTV 13 9)

  assertEqual "VS_ED (14,14) - trivial"
    (trivialCrossSet (ldSize (tcLD tc)))
    (getCrossSet boardV 14 14)
  assertEqual "VS_ED (14,14) cross score" 0 (getCrossScore boardV 14 14)

  assertEqual "VS_ED transposed (14,14) - trivial"
    (trivialCrossSet (ldSize (tcLD tc)))
    (getCrossSet boardTV 14 14)
  assertEqual "VS_ED transposed (14,14) cross score" 0 (getCrossScore boardTV 14 14)

  assertEqual "VS_ED (12,12) - empty"
    (stringToCrossSet (tcLD tc) "")
    (getCrossSet boardV 12 12)
  assertEqual "VS_ED (12,12) cross score" 0 (getCrossScore boardV 12 12)

  assertEqual "VS_ED transposed (12,12) - empty"
    (stringToCrossSet (tcLD tc) "")
    (getCrossSet boardTV 12 12)
  assertEqual "VS_ED transposed (12,12) cross score" 0 (getCrossScore boardTV 12 12)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Loading test configuration..."
  tcNWL20 <- loadTestConfig "NWL20"
  putStrLn $ "Loaded NWL20 KWG with " ++ show (numNodes (tcKWG tcNWL20)) ++ " nodes"

  putStrLn "\n=== Cross Set Tests (NWL20) ==="
  testCrossSetLoadedGame tcNWL20
  testCrossSetEdges tcNWL20
  testCrossSetBetween tcNWL20
  testCrossSetVsEd tcNWL20

  putStrLn "\n=== All cross set tests passed ==="
  exitSuccess
