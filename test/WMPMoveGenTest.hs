-- | WMP Move Generation tests
-- Ported from MAGPIE's wmp_move_gen_test.c
module Main where

import Magpie.WMPMoveGen
import Magpie.WMP
import Magpie.BitRack
import Magpie.Equity (Equity(..))
import Magpie.Types (MachineLetter(..), Rack, emptyRack, rackAddLetter)
import Magpie.LetterDistribution (defaultEnglishLD, ldFromChar, ldSize)

import Control.Monad (when, forM_)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Vector.Unboxed as VU

-- | Convert string to rack
stringToRack :: String -> Rack
stringToRack s =
  let distSize = ldSize defaultEnglishLD
      addChar c r = case ldFromChar defaultEnglishLD c of
        Just ml -> rackAddLetter ml r
        Nothing -> r
  in foldr addChar (emptyRack distSize) s

-- | Convert string to MachineLetter
charToML :: Char -> MachineLetter
charToML c = case ldFromChar defaultEnglishLD c of
  Just ml -> ml
  Nothing -> MachineLetter 0

-- | Equity from int (1000x fixed-point)
intToEquity :: Int -> Equity
intToEquity n = Equity (fromIntegral n * 1000)

-- | Set dummy leave values: empty=0, 1-tile=1000, 2-tile=2000, etc.
dummyLeaveValue :: Int -> Equity
dummyLeaveValue leaveIdx =
  let bitsSet = popCount leaveIdx
  in intToEquity bitsSet
  where
    popCount 0 = 0
    popCount n = (n `mod` 2) + popCount (n `div` 2)

-- | Test inactive WMP move gen
testWmpMoveGenInactive :: IO Bool
testWmpMoveGenInactive = do
  putStrLn "Testing inactive WMP move gen..."
  let wmg = wmpMoveGenInit Nothing (emptyRack 27)
  if wmpMoveGenIsActive wmg
    then do
      putStrLn "  FAIL: WMP move gen should be inactive with no WMP"
      return False
    else do
      putStrLn "  PASS: WMP move gen is inactive without WMP"
      return True

-- | Test nonplaythrough existence with VIVIFIC rack
testNonplaythroughExistence :: WMP -> IO Bool
testNonplaythroughExistence wmp = do
  putStrLn "Testing nonplaythrough existence (VIVIFIC)..."

  let rack = stringToRack "VIVIFIC"
      wmg0 = wmpMoveGenInit (Just wmp) rack
      wmg1 = wmpMoveGenResetPlaythrough wmg0

  -- Check active and no playthrough
  when (not $ wmpMoveGenIsActive wmg1) $ do
    putStrLn "  FAIL: WMP move gen should be active"

  when (wmpMoveGenHasPlaythrough wmg1) $ do
    putStrLn "  FAIL: Should have no playthrough initially"

  -- Check existence without checking leaves
  let wmg2 = wmpMoveGenCheckNonplaythroughExistence False (const (Equity 0)) wmg1

  -- IF is a 2-letter word
  let has2 = wmpMoveGenNonplaythroughWordOfLengthExists 2 wmg2
  when (not has2) $ putStrLn "  FAIL: Should have 2-letter word (IF)"

  -- No 3, 4, 5, or 6 letter words
  let no3to6 = all (not . flip wmpMoveGenNonplaythroughWordOfLengthExists wmg2) [3..6]
  when (not no3to6) $ putStrLn "  FAIL: Should have no 3-6 letter words"

  -- VIVIFIC is a 7-letter word
  let has7 = wmpMoveGenNonplaythroughWordOfLengthExists 7 wmg2
  when (not has7) $ putStrLn "  FAIL: Should have 7-letter word (VIVIFIC)"

  -- Check with leave values
  let wmg3 = wmpMoveGenCheckNonplaythroughExistence True dummyLeaveValue wmg1
  let bestLeaves = wmpMoveGenGetNonplaythroughBestLeaveValues wmg3

  -- Check best leaves for words that exist
  let checkLeave wordLen = do
        let leaveSize = 7 - wordLen
        if wmpMoveGenNonplaythroughWordOfLengthExists wordLen wmg3
          then do
            let expected = intToEquity leaveSize
                actual = bestLeaves VU.! leaveSize
            when (actual /= expected) $
              putStrLn $ "  FAIL: Best leave for " ++ show wordLen ++ "-letter word should be " ++
                        show expected ++ " but got " ++ show actual
          else return ()

  forM_ [2..7] checkLeave

  putStrLn "  PASS: Nonplaythrough existence tests"
  return (has2 && no3to6 && has7)

-- | Test playthrough bingo existence with CHEESE? rack
testPlaythroughBingoExistence :: WMP -> IO Bool
testPlaythroughBingoExistence wmp = do
  putStrLn "Testing playthrough bingo existence (CHEESE?)..."

  let rack = stringToRack "CHEESE?"
      wmg0 = wmpMoveGenInit (Just wmp) rack
      wmg1 = wmpMoveGenResetPlaythrough wmg0

  when (not $ wmpMoveGenIsActive wmg1) $ do
    putStrLn "  FAIL: WMP move gen should be active"

  when (wmpMoveGenHasPlaythrough wmg1) $ do
    putStrLn "  FAIL: Should have no playthrough initially"

  -- Add letter N (shadowing left)
  let wmg2 = wmpMoveGenAddPlaythroughLetter (charToML 'N') wmg1

  when (not $ wmpMoveGenHasPlaythrough wmg2) $ do
    putStrLn "  FAIL: Should have playthrough after adding N"

  -- CHEESE? + N = ENCHEErS (should exist)
  let exists1 = wmpMoveGenCheckPlaythroughFullRackExistence wmg2
  when (not exists1) $
    putStrLn "  FAIL: CHEESE? + N should form a bingo"

  -- Save state and add P (shadowing right)
  let wmg3 = wmpMoveGenSavePlaythroughState wmg2
      wmg4 = wmpMoveGenAddPlaythroughLetter (charToML 'P') wmg3

  -- CHEESE? + NP = NiPCHEESE/PENnEECHS (should exist)
  let exists2 = wmpMoveGenCheckPlaythroughFullRackExistence wmg4
  when (not exists2) $
    putStrLn "  FAIL: CHEESE? + NP should form a bingo"

  -- Add Q (no bingo)
  let wmg5 = wmpMoveGenAddPlaythroughLetter (charToML 'Q') wmg4
      exists3 = wmpMoveGenCheckPlaythroughFullRackExistence wmg5
  when exists3 $
    putStrLn "  FAIL: CHEESE? + NPQ should NOT form a bingo"

  -- Restore to N, add I
  let wmg6 = wmpMoveGenRestorePlaythroughState wmg4
      wmg7 = wmpMoveGenAddPlaythroughLetter (charToML 'I') wmg6

  -- CHEESE? + NI = NIpCHEESE (should exist)
  let exists4 = wmpMoveGenCheckPlaythroughFullRackExistence wmg7
  when (not exists4) $
    putStrLn "  FAIL: CHEESE? + NI should form a bingo"

  -- Save and add P
  let wmg8 = wmpMoveGenSavePlaythroughState wmg7
      wmg9 = wmpMoveGenAddPlaythroughLetter (charToML 'P') wmg8

  -- CHEESE? + NIP = NIPCHEESEs (should exist)
  let exists5 = wmpMoveGenCheckPlaythroughFullRackExistence wmg9
  when (not exists5) $
    putStrLn "  FAIL: CHEESE? + NIP should form a bingo"

  putStrLn $ "  Results: N=" ++ show exists1 ++ ", NP=" ++ show exists2 ++
             ", NPQ=" ++ show exists3 ++ ", NI=" ++ show exists4 ++ ", NIP=" ++ show exists5

  let allOk = exists1 && exists2 && not exists3 && exists4 && exists5
  if allOk
    then putStrLn "  PASS: Playthrough bingo existence tests"
    else putStrLn "  FAIL: Some playthrough tests failed"

  return allOk

-- | Run all WMP move gen tests
runWMPMoveGenTests :: FilePath -> IO Bool
runWMPMoveGenTests wmpPath = do
  putStrLn $ "Loading WMP from: " ++ wmpPath
  wmp <- loadWMP wmpPath
  putStrLn $ "  Loaded WMP version " ++ show (wmpVersion wmp)
  putStrLn ""

  results <- sequence
    [ testWmpMoveGenInactive
    , testNonplaythroughExistence wmp
    , testPlaythroughBingoExistence wmp
    ]

  return (and results)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [] -> do
      -- Try default paths
      let defaultPaths =
            [ "data/lexica/CSW21.wmp"
            , "data/lexica/CSW24.wmp"
            , "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/lexica/CSW24.wmp"
            ]
      existingPaths <- filterM doesFileExist defaultPaths
      case existingPaths of
        [] -> do
          putStrLn "No WMP file found. Please provide a WMP file path as argument."
          putStrLn "Example: cabal run wmp-movegen-test -- /path/to/CSW24.wmp"
          exitFailure
        (path:_) -> do
          ok <- runWMPMoveGenTests path
          if ok
            then do
              putStrLn "\nAll WMP move gen tests passed!"
              exitSuccess
            else do
              putStrLn "\nSome WMP move gen tests failed!"
              exitFailure
    [wmpPath] -> do
      exists <- doesFileExist wmpPath
      if exists
        then do
          ok <- runWMPMoveGenTests wmpPath
          if ok
            then do
              putStrLn "\nAll WMP move gen tests passed!"
              exitSuccess
            else do
              putStrLn "\nSome WMP move gen tests failed!"
              exitFailure
        else do
          putStrLn $ "WMP file not found: " ++ wmpPath
          exitFailure
    _ -> do
      putStrLn "Usage: wmp-movegen-test [path-to-wmp-file]"
      exitFailure

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
  b <- p x
  rest <- filterM p xs
  return $ if b then x:rest else rest
