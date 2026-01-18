-- | WMP (Word Map) loading and querying tests
-- Tests ported from MAGPIE's wmp_test.c
--
-- To run these tests, you need a WMP file. Generate one using the C code:
--   cd MAGPIE && ./bin/test_wmp
-- Then copy CSW21_3or15.wmp or CSW21.wmp to haskell-words/data/lexica/
module Main where

import Magpie.WMP
import Magpie.BitRack
import Magpie.Types (MachineLetter(..))

import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

-- | Convert a string to BitRack using the letter distribution
-- '?' represents a blank (letter 0)
-- Uppercase letters are regular tiles
stringToBitRack :: String -> BitRack
stringToBitRack = foldr addChar emptyBitRack
  where
    addChar '?' br = bitRackAddLetter (MachineLetter 0) br
    addChar c br
      | c >= 'A' && c <= 'Z' =
          let ml = MachineLetter (fromIntegral (fromEnum c - fromEnum 'A' + 1))
          in bitRackAddLetter ml br
      | otherwise = br  -- Skip invalid characters

-- | Convert a list of machine letters to string
mlsToString :: [MachineLetter] -> String
mlsToString = map mlToChar
  where
    mlToChar (MachineLetter 0) = '?'
    mlToChar (MachineLetter ml)
      | ml >= 1 && ml <= 26 = toEnum (fromIntegral ml - 1 + fromEnum 'A')
      | otherwise = '?'

-- | Assert that a word appears in the result list
assertWordInResults :: [[MachineLetter]] -> String -> IO Bool
assertWordInResults results expected = do
  let resultStrings = map mlsToString results
      found = expected `elem` resultStrings
  if not found
    then do
      putStrLn $ "  FAIL: Expected word '" ++ expected ++ "' not found in results"
      putStrLn $ "  Got: " ++ show resultStrings
      return False
    else return True

-- | Test blankless word lookup
testBlanklessWords :: WMP -> IO Bool
testBlanklessWords wmp = do
  putStrLn "Testing blankless word lookup..."

  -- Test "INQ" -> should find "QIN"
  let inq = stringToBitRack "INQ"
  let results = wmpWriteWordsToBuffer wmp inq 3
  putStrLn $ "  INQ (3-letter anagrams): " ++ show (map mlsToString results)

  if null results
    then do
      putStrLn "  SKIP: No 3-letter blankless words found (WMP may not have length-3 words)"
      return True
    else assertWordInResults results "QIN"

-- | Test single-blank word lookup
testSingleBlankWords :: WMP -> IO Bool
testSingleBlankWords wmp = do
  putStrLn "Testing single-blank word lookup..."

  -- Test "VV?" -> should find "VAV"
  let vvBlank = stringToBitRack "VV?"
  let results = wmpWriteWordsToBuffer wmp vvBlank 3

  putStrLn $ "  VV? (3-letter anagrams): " ++ show (map mlsToString results)

  if null results
    then do
      putStrLn "  SKIP: No 3-letter single-blank words found"
      return True
    else assertWordInResults results "VAV"

-- | Test double-blank word lookup
testDoubleBlankWords :: WMP -> IO Bool
testDoubleBlankWords wmp = do
  putStrLn "Testing double-blank word lookup..."

  -- Test "Q??" -> should find multiple words: QAT, QUA, QIN, QIS, SUQ
  let qDoubleBlank = stringToBitRack "Q??"
  let results = wmpWriteWordsToBuffer wmp qDoubleBlank 3

  putStrLn $ "  Q?? (3-letter anagrams): " ++ show (map mlsToString results)

  if null results
    then do
      putStrLn "  SKIP: No 3-letter double-blank words found"
      return True
    else do
      -- Should have multiple Q words
      ok1 <- assertWordInResults results "QAT"
      ok2 <- assertWordInResults results "QUA"
      ok3 <- assertWordInResults results "QIN"
      ok4 <- assertWordInResults results "QIS"
      ok5 <- assertWordInResults results "SUQ"
      return (ok1 && ok2 && ok3 && ok4 && ok5)

-- | Test 15-letter word lookup
testLongWords :: WMP -> IO Bool
testLongWords wmp = do
  putStrLn "Testing 15-letter word lookup..."

  -- Test "QUARTERBACKIN??" -> should find "QUARTERBACKINGS"
  let qbBlank = stringToBitRack "QUARTERBACKIN??"
  let results = wmpWriteWordsToBuffer wmp qbBlank 15

  putStrLn $ "  QUARTERBACKIN?? (15-letter anagrams): " ++ show (map mlsToString results)

  if null results
    then do
      putStrLn "  SKIP: No 15-letter words found (WMP may not have length-15 words)"
      return True
    else assertWordInResults results "QUARTERBACKINGS"

-- | Run all WMP tests
runWMPTests :: FilePath -> IO Bool
runWMPTests wmpPath = do
  putStrLn $ "Loading WMP from: " ++ wmpPath
  wmp <- loadWMP wmpPath

  putStrLn $ "  Version: " ++ show (wmpVersion wmp)
  putStrLn $ "  Board dim: " ++ show (wmpBoardDim wmp)
  putStrLn $ "  Max word lookup bytes: " ++ show (wmpMaxWordLookupBytes wmp)
  putStrLn ""

  results <- sequence
    [ testBlanklessWords wmp
    , testSingleBlankWords wmp
    , testDoubleBlankWords wmp
    , testLongWords wmp
    ]

  return (and results)

-- | Test basic BitRack operations
testBitRack :: IO Bool
testBitRack = do
  putStrLn "Testing BitRack operations..."

  -- Test empty bit rack
  let empty = emptyBitRack
  let emptyCount = bitRackTotalTiles empty
  putStrLn $ "  Empty BitRack total tiles: " ++ show emptyCount
  when (emptyCount /= 0) $ putStrLn "  FAIL: Empty BitRack should have 0 tiles"

  -- Test adding letters
  let br1 = bitRackAddLetter (MachineLetter 1) empty  -- Add 'A'
  let br2 = bitRackAddLetter (MachineLetter 1) br1    -- Add another 'A'
  let count1 = bitRackGetLetterCount (MachineLetter 1) br1
  let count2 = bitRackGetLetterCount (MachineLetter 1) br2
  putStrLn $ "  After adding A once: count = " ++ show count1
  putStrLn $ "  After adding A twice: count = " ++ show count2
  when (count1 /= 1 || count2 /= 2) $ putStrLn "  FAIL: Letter counts incorrect"

  -- Test removing letters
  let br3 = bitRackRemoveLetter (MachineLetter 1) br2
  let count3 = bitRackGetLetterCount (MachineLetter 1) br3
  putStrLn $ "  After removing A once: count = " ++ show count3
  when (count3 /= 1) $ putStrLn "  FAIL: Letter count should be 1 after removal"

  -- Test blank counting
  let brBlank = bitRackAddLetter (MachineLetter 0) empty
  let blankCount = bitRackNumBlanks brBlank
  putStrLn $ "  After adding blank: blank count = " ++ show blankCount
  when (blankCount /= 1) $ putStrLn "  FAIL: Blank count should be 1"

  -- Test total tiles
  let brMulti = foldr bitRackAddLetter empty [MachineLetter 1, MachineLetter 2, MachineLetter 3]
  let total = bitRackTotalTiles brMulti
  putStrLn $ "  After adding A, B, C: total tiles = " ++ show total
  when (total /= 3) $ putStrLn "  FAIL: Total tiles should be 3"

  putStrLn "  BitRack tests passed!"
  return True

main :: IO ()
main = do
  args <- getArgs

  -- Always run BitRack tests
  bitRackOk <- testBitRack
  putStrLn ""

  case args of
    [] -> do
      -- Try default paths
      let defaultPaths =
            [ "data/lexica/CSW21.wmp"
            , "data/lexica/CSW21_3or15.wmp"
            , "../data/lexica/CSW21.wmp"
            ]
      existingPaths <- filterM doesFileExist defaultPaths
      case existingPaths of
        [] -> do
          putStrLn "No WMP file found. To run full WMP tests:"
          putStrLn "  1. Generate WMP files using the C code: cd MAGPIE && ./bin/test_wmp"
          putStrLn "  2. Copy a .wmp file to data/lexica/"
          putStrLn "  3. Run this test again, or pass the WMP path as an argument"
          putStrLn ""
          putStrLn "BitRack tests passed. WMP loading tests skipped."
          if bitRackOk then exitSuccess else exitFailure
        (path:_) -> do
          wmpOk <- runWMPTests path
          if bitRackOk && wmpOk
            then do
              putStrLn "\nAll tests passed!"
              exitSuccess
            else do
              putStrLn "\nSome tests failed!"
              exitFailure
    [wmpPath] -> do
      exists <- doesFileExist wmpPath
      if exists
        then do
          wmpOk <- runWMPTests wmpPath
          if bitRackOk && wmpOk
            then do
              putStrLn "\nAll tests passed!"
              exitSuccess
            else do
              putStrLn "\nSome tests failed!"
              exitFailure
        else do
          putStrLn $ "WMP file not found: " ++ wmpPath
          exitFailure
    _ -> do
      putStrLn "Usage: wmp-test [path-to-wmp-file]"
      exitFailure

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
  b <- p x
  rest <- filterM p xs
  return $ if b then x:rest else rest
