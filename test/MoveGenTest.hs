{-# LANGUAGE OverloadedStrings #-}

-- | Move generation tests ported from MAGPIE
module Main where

import Magpie
import Magpie.Types
import Magpie.KWG
import Magpie.LetterDistribution
import Magpie.Board (loadCGP, standardBoard)
import Magpie.MoveGen

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when, unless, forM_)

-- | Test configuration for a specific lexicon
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

-- | Count scoring (tile placement) moves
countScoringPlays :: [Move] -> Int
countScoringPlays = length . filter (\m -> moveType m == TilePlacement)

-- | Count non-scoring (pass/exchange) moves
countNonScoringPlays :: [Move] -> Int
countNonScoringPlays = length . filter (\m -> moveType m /= TilePlacement)

-- | Assert a condition with a message
assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = do
  putStrLn $ "FAIL: " ++ msg
  exitFailure

-- | Assert equality with message
assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual expected actual msg
  | expected == actual = return ()
  | otherwise = do
      putStrLn $ "FAIL: " ++ msg
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Actual:   " ++ show actual
      exitFailure

-- | Format a move for display
showMove :: LetterDistribution -> Move -> String
showMove ld m = case moveType m of
  Pass -> "(pass)"
  Exchange -> "(exch " ++ ldToString ld (moveTiles m) ++ ")"
  TilePlacement ->
    let col = [toEnum (fromEnum 'A' + moveCol m)]
        row = show (moveRow m + 1)
        dirStr = if moveDir m == Vertical then "v" else ""
        tiles = ldToString ld (moveTiles m)
    in col ++ row ++ dirStr ++ " " ++ tiles ++ " " ++ show (moveScore m)

-- | Generate moves for a rack string on a board
genMovesForRack :: TestConfig -> Board -> String -> [Move]
genMovesForRack tc board rackStr =
  case ldFromString (tcLD tc) rackStr of
    Nothing -> error $ "Could not parse rack: " ++ rackStr
    Just mls ->
      let rack = rackFromList (ldSize (tcLD tc)) mls
      in generateMoves (tcKWG tc) (tcLD tc) board rack

-- | Get top N scoring moves sorted by score descending
topMoves :: Int -> [Move] -> [Move]
topMoves n = take n . sortBy (comparing (Down . moveScore)) . filter (\m -> moveType m == TilePlacement)

--------------------------------------------------------------------------------
-- CGP Test Constants (from MAGPIE test_constants.h)
--------------------------------------------------------------------------------

-- | VS_ED CGP position
vsEd :: String
vsEd = "14E/14N/14d/14U/4GLOWS5R/8PET3E/7FAXING1R/6JAY1TEEMS/2B2BOY4N2/2L1DOE5U2/2ANEW5PI2/2MO1LEU3ON2/2EH7HE2/15/15"

-- | VS_JEREMY CGP position
vsJeremy :: String
vsJeremy = "7N6M/5ZOON4AA/7B5UN/2S4L3LADY/2T4E2QI1I1/2A2PORN3NOR/2BICE2AA1DA1E/6GUVS1OP1F/8ET1LA1U/5J3R1E1UT/4VOTE1I1R1NE/5G1MICKIES1/6FE1T1THEW/6OR3E1XI/6OY6G"

-- | VS_OXY CGP position
vsOxy :: String
vsOxy = "1PACIFYING5/1IS12/YE13/1REQUALIFIED3/H1L12/EDS12/NO3T9/1RAINWASHING3/UM3O9/T2E1O9/1WAKEnERS6/1OnETIME7/OOT2E1B7/N6U7/1JACULATING4"

-- | VS_MATT CGP position
vsMatt :: String
vsMatt = "7ZEP1F3/1FLUKY3R1R3/5EX2A1U3/2SCARIEST1I3/9TOT3/6GO1LO4/6OR1ETA3/6JABS1b3/5QI4A3/5I1N3N3/3ReSPOND1D3/1HOE3V3O3/1ENCOMIA3N3/7T7/3VENGED6"

-- | MANY_MOVES CGP position (generates 238895 moves)
manyMoves :: String
manyMoves = "7P7/7A7/7R7/7T7/7E7/7R7/4P2RETRACED/1ORDINEE3S3/4C6T3/4T6O3/4U6N3/4R6I3/4A6E3/4L6S3/15"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGenerateEmptyBoard
-- NWL20: DEGORV? -> 3307 scoring plays, 128 non-scoring
-- CSW21: DEGORV? -> 4497 scoring plays, 128 non-scoring
--------------------------------------------------------------------------------
testGenerateEmptyBoard :: TestConfig -> IO ()
testGenerateEmptyBoard tc = do
  let board = standardBoard
      moves = genMovesForRack tc board "DEGORV?"
      scoring = countScoringPlays moves
      nonScoring = countNonScoringPlays moves
      expected = if tcName tc == "NWL20" then 3307 else 4497

  putStrLn $ "  " ++ tcName tc ++ " DEGORV? scoring: " ++ show scoring ++ " (expected " ++ show expected ++ ")"
  assertEqual expected scoring $ tcName tc ++ " DEGORV? scoring plays"
  assertEqual 128 nonScoring $ tcName tc ++ " DEGORV? non-scoring plays"

  -- Top move should score 80
  case topMoves 1 moves of
    (top:_) -> assertEqual 80 (moveScore top) $ tcName tc ++ " top move score"
    [] -> assert False "No moves generated"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGenerateEmptyBoard without blank
-- NWL20: DEGORV -> 156 scoring plays
-- CSW21: DEGORV -> 216 scoring plays
--------------------------------------------------------------------------------
testGenerateEmptyBoardNoBlank :: TestConfig -> IO ()
testGenerateEmptyBoardNoBlank tc = do
  let board = standardBoard
      moves = genMovesForRack tc board "DEGORV"
      scoring = countScoringPlays moves
      expected = if tcName tc == "NWL20" then 156 else 216

  putStrLn $ "  " ++ tcName tc ++ " DEGORV scoring: " ++ show scoring ++ " (expected " ++ show expected ++ ")"
  assertEqual expected scoring $ tcName tc ++ " DEGORV scoring plays"

--------------------------------------------------------------------------------
-- Test single tile on empty board (should be 0 scoring plays)
--------------------------------------------------------------------------------
testSingleTileEmptyBoard :: TestConfig -> IO ()
testSingleTileEmptyBoard tc = do
  let board = standardBoard
      moves = genMovesForRack tc board "A"
      scoring = countScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " single tile 'A' scoring: " ++ show scoring ++ " (expected 0)"
  assertEqual 0 scoring $ tcName tc ++ " single tile scoring plays"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGenAllMovesSingleTile
-- VS_MATT with rack "A" -> 24 scoring plays
--------------------------------------------------------------------------------
testGenAllMovesSingleTile :: TestConfig -> IO ()
testGenAllMovesSingleTile tc = do
  let board = loadCGP (tcLD tc) vsMatt
      moves = genMovesForRack tc board "A"
      scoring = countScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " VS_MATT rack A scoring: " ++ show scoring ++ " (expected 24)"
  assertEqual 24 scoring $ tcName tc ++ " VS_MATT single tile scoring plays"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGenAllMovesFullRack
-- VS_MATT with rack "AABDELT" -> 667 scoring plays, 96 non-scoring
--------------------------------------------------------------------------------
testGenAllMovesFullRack :: TestConfig -> IO ()
testGenAllMovesFullRack tc = do
  let board = loadCGP (tcLD tc) vsMatt
      moves = genMovesForRack tc board "AABDELT"
      scoring = countScoringPlays moves
      nonScoring = countNonScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " VS_MATT rack AABDELT scoring: " ++ show scoring ++ " (expected 667)"
  putStrLn $ "  " ++ tcName tc ++ " VS_MATT rack AABDELT non-scoring: " ++ show nonScoring ++ " (expected 96)"
  assertEqual 667 scoring $ tcName tc ++ " VS_MATT AABDELT scoring plays"
  assertEqual 96 nonScoring $ tcName tc ++ " VS_MATT AABDELT non-scoring plays"

  -- Check top scores
  let top10 = topMoves 10 moves
      expectedScores = [38, 36, 36, 34, 34, 33, 30, 30, 30, 28]
  forM_ (zip [0..] expectedScores) $ \(i, expScore) ->
    when (i < length top10) $
      assertEqual expScore (moveScore (top10 !! i)) $
        tcName tc ++ " VS_MATT AABDELT move " ++ show i ++ " score"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGenAllMovesFullRackAgain
-- VS_ED with rack "AFGIIIS" -> 219 scoring plays, 64 non-scoring
--------------------------------------------------------------------------------
testGenAllMovesFullRackAgain :: TestConfig -> IO ()
testGenAllMovesFullRackAgain tc = do
  let board = loadCGP (tcLD tc) vsEd
      moves = genMovesForRack tc board "AFGIIIS"
      scoring = countScoringPlays moves
      nonScoring = countNonScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " VS_ED rack AFGIIIS scoring: " ++ show scoring ++ " (expected 219)"
  assertEqual 219 scoring $ tcName tc ++ " VS_ED AFGIIIS scoring plays"
  assertEqual 64 nonScoring $ tcName tc ++ " VS_ED AFGIIIS non-scoring plays"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGenAllMovesSingleBlank
-- VS_ED with rack "?" -> 169 scoring plays, 2 non-scoring
--------------------------------------------------------------------------------
testGenAllMovesSingleBlank :: TestConfig -> IO ()
testGenAllMovesSingleBlank tc = do
  let board = loadCGP (tcLD tc) vsEd
      moves = genMovesForRack tc board "?"
      scoring = countScoringPlays moves
      nonScoring = countNonScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " VS_ED rack ? scoring: " ++ show scoring ++ " (expected 169)"
  assertEqual 169 scoring $ tcName tc ++ " VS_ED single blank scoring plays"
  assertEqual 2 nonScoring $ tcName tc ++ " VS_ED single blank non-scoring plays"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGenAllMovesTwoBlanksOnly
-- VS_ED with rack "??" -> 1961 scoring plays, 3 non-scoring
--------------------------------------------------------------------------------
testGenAllMovesTwoBlanksOnly :: TestConfig -> IO ()
testGenAllMovesTwoBlanksOnly tc = do
  let board = loadCGP (tcLD tc) vsEd
      moves = genMovesForRack tc board "??"
      scoring = countScoringPlays moves
      nonScoring = countNonScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " VS_ED rack ?? scoring: " ++ show scoring ++ " (expected 1961)"
  assertEqual 1961 scoring $ tcName tc ++ " VS_ED two blanks scoring plays"
  assertEqual 3 nonScoring $ tcName tc ++ " VS_ED two blanks non-scoring plays"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGenAllMovesWithBlanks
-- VS_JEREMY with rack "DDESW??" -> 8285 scoring plays
-- (1 non-scoring when exchange not possible)
--------------------------------------------------------------------------------
testGenAllMovesWithBlanks :: TestConfig -> IO ()
testGenAllMovesWithBlanks tc = do
  let board = loadCGP (tcLD tc) vsJeremy
      moves = genMovesForRack tc board "DDESW??"
      scoring = countScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " VS_JEREMY rack DDESW?? scoring: " ++ show scoring ++ " (expected 8285)"
  assertEqual 8285 scoring $ tcName tc ++ " VS_JEREMY DDESW?? scoring plays"

  -- Top move should be hEaDW(OR)DS for 106
  let top = topMoves 1 moves
  case top of
    (m:_) -> assertEqual 106 (moveScore m) $ tcName tc ++ " VS_JEREMY top move score"
    [] -> assert False "No moves generated"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGiantTwentySevenTimer
-- VS_OXY with rack "ABEOPXZ" -> 513 scoring plays, 128 non-scoring
--------------------------------------------------------------------------------
testGiantTwentySevenTimer :: TestConfig -> IO ()
testGiantTwentySevenTimer tc = do
  let board = loadCGP (tcLD tc) vsOxy
      moves = genMovesForRack tc board "ABEOPXZ"
      scoring = countScoringPlays moves
      nonScoring = countNonScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " VS_OXY rack ABEOPXZ scoring: " ++ show scoring ++ " (expected 513)"
  assertEqual 513 scoring $ tcName tc ++ " VS_OXY ABEOPXZ scoring plays"
  assertEqual 128 nonScoring $ tcName tc ++ " VS_OXY ABEOPXZ non-scoring plays"

  -- Top move should be OX(Y)P(HEN)B(UT)AZ(ON)E for 1780
  let top = topMoves 1 moves
  case top of
    (m:_) -> assertEqual 1780 (moveScore m) $ tcName tc ++ " VS_OXY OXYPHENBUTAZONE score"
    [] -> assert False "No moves generated"

--------------------------------------------------------------------------------
-- MAGPIE Test: TestGenerateNoPlays
-- VS_JEREMY with rack "V" -> 0 scoring plays (V can't be played)
--------------------------------------------------------------------------------
testGenerateNoPlays :: TestConfig -> IO ()
testGenerateNoPlays tc = do
  let board = loadCGP (tcLD tc) vsJeremy
      moves = genMovesForRack tc board "V"
      scoring = countScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " VS_JEREMY rack V scoring: " ++ show scoring ++ " (expected 0)"
  assertEqual 0 scoring $ tcName tc ++ " VS_JEREMY V scoring plays (no valid plays)"

--------------------------------------------------------------------------------
-- MAGPIE Test: MovegenManyMoves (CSW21)
-- MANY_MOVES position with rack "AEINS??" -> 238895 scoring plays
--------------------------------------------------------------------------------
testMovegenManyMoves :: TestConfig -> IO ()
testMovegenManyMoves tc = do
  putStrLn $ "  Testing MANY_MOVES with " ++ tcName tc ++ "..."
  let board = loadCGP (tcLD tc) manyMoves
      moves = genMovesForRack tc board "AEINS??"
      scoring = countScoringPlays moves
      nonScoring = countNonScoringPlays moves

  putStrLn $ "  " ++ tcName tc ++ " MANY_MOVES scoring: " ++ show scoring ++ " (expected 238895)"
  assertEqual 238895 scoring $ tcName tc ++ " MANY_MOVES AEINS?? scoring plays"
  assertEqual 96 nonScoring $ tcName tc ++ " MANY_MOVES AEINS?? non-scoring plays"

--------------------------------------------------------------------------------
-- Run all empty board tests for a lexicon
--------------------------------------------------------------------------------
runEmptyBoardTests :: TestConfig -> IO ()
runEmptyBoardTests tc = do
  putStrLn $ "\n=== Empty board tests (" ++ tcName tc ++ ") ==="
  testGenerateEmptyBoard tc
  testGenerateEmptyBoardNoBlank tc
  testSingleTileEmptyBoard tc

--------------------------------------------------------------------------------
-- Run all macondo-style tests for NWL20 (original MAGPIE test lexicon)
--------------------------------------------------------------------------------
runMacondoTests :: TestConfig -> IO ()
runMacondoTests tc = do
  putStrLn $ "\n=== Macondo-style tests (" ++ tcName tc ++ ") ==="
  testGenAllMovesSingleTile tc
  testGenAllMovesFullRack tc
  testGenAllMovesFullRackAgain tc
  testGenAllMovesSingleBlank tc
  testGenAllMovesTwoBlanksOnly tc
  testGenAllMovesWithBlanks tc
  testGiantTwentySevenTimer tc
  testGenerateNoPlays tc

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Loading test configurations..."

  -- Test with NWL20 (original MAGPIE test lexicon)
  tcNWL20 <- loadTestConfig "NWL20"
  putStrLn $ "Loaded NWL20 KWG with " ++ show (numNodes (tcKWG tcNWL20)) ++ " nodes"

  -- Test with CSW21 (larger lexicon)
  tcCSW21 <- loadTestConfig "CSW21"
  putStrLn $ "Loaded CSW21 KWG with " ++ show (numNodes (tcKWG tcCSW21)) ++ " nodes"

  -- Run tests for both lexicons
  runEmptyBoardTests tcNWL20
  runEmptyBoardTests tcCSW21

  -- Macondo-style tests (use NWL20)
  runMacondoTests tcNWL20

  -- Many moves test (use CSW21)
  putStrLn "\n=== Many moves test (CSW21) ==="
  testMovegenManyMoves tcCSW21

  putStrLn "\n=== All tests passed ==="
  exitSuccess
