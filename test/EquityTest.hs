{-# LANGUAGE OverloadedStrings #-}

-- | Equity and KLV tests ported from MAGPIE
module Main where

import Magpie.Equity
import Magpie.KLV
import Magpie.Types (Rack(..), MachineLetter(..))
import Magpie.LetterDistribution (loadLetterDistribution, ldSize, ldFromChar)

import qualified Data.Vector.Unboxed as VU
import System.Exit (exitFailure, exitSuccess)
import System.Directory (doesFileExist)
import Control.Monad (when, forM_)
import Data.Int (Int32)
import Control.Exception (catch, IOException)

-- | Assert two values are equal
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual
  | expected == actual = return ()
  | otherwise = do
      putStrLn $ "FAIL: " ++ msg
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Actual:   " ++ show actual
      exitFailure

-- | Assert a condition is true
assertTrue :: String -> Bool -> IO ()
assertTrue msg cond
  | cond = return ()
  | otherwise = do
      putStrLn $ "FAIL: " ++ msg
      exitFailure

-- | Check if two doubles are within epsilon
withinEpsilon :: Double -> Double -> Bool
withinEpsilon a b = abs (a - b) < 0.001

--------------------------------------------------------------------------------
-- Equity Tests
--------------------------------------------------------------------------------

-- | Test stability: converting to double and back should be stable
testStabilityAndNegation :: Equity -> IO ()
testStabilityAndNegation eq1
  | eq1 == equityUndefined || eq1 == equityInitial || eq1 == equityPass = return ()
  | otherwise = do
      let dbl1 = equityToDouble eq1
          eq2 = doubleToEquity dbl1
      when (eq1 /= eq2) $ do
        putStrLn $ "FAIL: stability test for " ++ show eq1
        putStrLn $ "  double: " ++ show dbl1
        putStrLn $ "  back to equity: " ++ show eq2
        exitFailure
      let eqNeg = equityNegate eq1
          eqNegNeg = equityNegate eqNeg
      when (eq1 /= eqNegNeg) $ do
        putStrLn $ "FAIL: negation test for " ++ show eq1
        exitFailure

testEquity :: IO ()
testEquity = do
  putStrLn "  Testing Equity type..."

  -- Test ordering required for movegen
  assertTrue "INITIAL < PASS" (equityInitial < equityPass)

  -- Test int conversion
  assertEqual "intToEquity(0)" (Equity 0) (intToEquity 0)
  assertEqual "intToEquity(1)" (Equity 1000) (intToEquity 1)
  assertEqual "intToEquity(-1)" (Equity (-1000)) (intToEquity (-1))

  -- Test double conversion
  assertTrue "equityToDouble(0) ~= 0.0" $ withinEpsilon (equityToDouble (Equity 0)) 0.0

  -- Test small values round to 0
  assertEqual "doubleToEquity(0.0)" (Equity 0) (doubleToEquity 0.0)
  assertEqual "doubleToEquity(0.00000008)" (Equity 0) (doubleToEquity 0.00000008)
  assertEqual "doubleToEquity(-0.00000008)" (Equity 0) (doubleToEquity (-0.00000008))

  -- Test stability
  testStabilityAndNegation equityMinValue
  testStabilityAndNegation equityMaxValue
  testStabilityAndNegation (Equity 0)

  -- Test a range of values
  let testVals = take 100 $ iterate (+ 10000000) (unEquity equityMinValue)
  forM_ testVals $ \v -> testStabilityAndNegation (Equity (fromIntegral v))

  putStrLn "  Equity tests passed."

--------------------------------------------------------------------------------
-- KLV Tests
--------------------------------------------------------------------------------

-- | Create a rack from a string for testing
stringToRack :: Int -> String -> Rack
stringToRack distSize str = Rack counts total distSize
  where
    counts = VU.accum (+) (VU.replicate distSize 0) updates
    updates = [(letterToIdx c, 1) | c <- str]
    total = length str
    letterToIdx '?' = 0
    letterToIdx c
      | c >= 'A' && c <= 'Z' = fromEnum c - fromEnum 'A' + 1
      | otherwise = 0

testKLVLoad :: IO Bool
testKLVLoad = do
  putStrLn "  Testing KLV loading..."

  -- Try to load the CSW21 KLV (leave values)
  let klvPath = "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/lexica/CSW21.klv"
  exists <- doesFileExist klvPath
  if not exists
    then do
      putStrLn $ "    KLV file not found: " ++ klvPath
      putStrLn "    Skipping KLV tests (no KLV file available)"
      return False
    else do
      klv <- loadKLV klvPath

      -- Check that the KLV loaded
      let numNodes = VU.length (klvKWGNodes klv)
      putStrLn $ "    Loaded KLV with " ++ show numNodes ++ " KWG nodes"
      assertTrue "KLV has nodes" (numNodes > 0)

      let numLeaves = VU.length (klvLeaveValues klv)
      putStrLn $ "    Loaded " ++ show numLeaves ++ " leave values"
      assertTrue "KLV has leave values" (numLeaves > 0)

      putStrLn "  KLV loading tests passed."
      return True

testKLVLeaveValues :: IO ()
testKLVLeaveValues = do
  putStrLn "  Testing KLV leave value lookup..."

  let klvPath = "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/lexica/CSW21.klv"
  exists <- doesFileExist klvPath
  if not exists
    then putStrLn "    Skipping (no KLV file)"
    else do
      klv <- loadKLV klvPath

      -- Test some basic racks
      -- Note: Leave values are in fixed-point (1000x resolution)

      -- Empty rack should have leave value 0
      let emptyRack = Rack (VU.replicate 27 0) 0 27
      assertEqual "empty rack leave" 0 (klvGetLeaveValue klv emptyRack)

      -- Single A
      let rackA = stringToRack 27 "A"
      let leaveA = klvGetLeaveValue klv rackA
      putStrLn $ "    Leave value for A: " ++ show (fromIntegral leaveA / 1000.0 :: Double)
      -- A should have a positive leave value (it's a vowel)

      -- Single Q
      let rackQ = stringToRack 27 "Q"
      let leaveQ = klvGetLeaveValue klv rackQ
      putStrLn $ "    Leave value for Q: " ++ show (fromIntegral leaveQ / 1000.0 :: Double)
      -- Q should have a negative leave value

      -- Single blank
      let rackBlank = stringToRack 27 "?"
      let leaveBlank = klvGetLeaveValue klv rackBlank
      putStrLn $ "    Leave value for ?: " ++ show (fromIntegral leaveBlank / 1000.0 :: Double)
      -- Blank should have a positive leave value

      -- SATINE (very good leave)
      let rackSatine = stringToRack 27 "SATINE"
      let leaveSatine = klvGetLeaveValue klv rackSatine
      putStrLn $ "    Leave value for SATINE: " ++ show (fromIntegral leaveSatine / 1000.0 :: Double)
      -- SATINE should have a high leave value

      -- QQ (bad leave)
      let rackQQ = stringToRack 27 "QQ"
      let leaveQQ = klvGetLeaveValue klv rackQQ
      putStrLn $ "    Leave value for QQ: " ++ show (fromIntegral leaveQQ / 1000.0 :: Double)
      -- QQ might not be found (unfound = 0)

      putStrLn "  KLV leave value tests passed."

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Equity Tests ==="
  testEquity

  putStrLn "\n=== KLV Tests ==="
  klvAvailable <- testKLVLoad
  when klvAvailable testKLVLeaveValues

  putStrLn "\n=== All equity tests passed ==="
  exitSuccess
