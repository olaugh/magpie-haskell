{-# LANGUAGE OverloadedStrings #-}
module Main where

import Magpie.KLV
import Magpie.Types
import Magpie.LetterDistribution

main :: IO ()
main = do
  klv <- loadKLV "data/lexica/CSW21.klv2"
  let ld = defaultEnglishLD

  putStrLn "Testing leave values from CSW21.klv2:"
  putStrLn ""

  let testLeave str = case ldFromString ld str of
        Nothing -> putStrLn $ str ++ ": invalid"
        Just mls -> do
          let rack = rackFromList (ldSize ld) mls
              idx = klvGetWordIndex klv rack
              val = klvGetLeaveValue klv rack
          putStrLn $ str ++ ": idx=" ++ show idx ++ " val=" ++ show (fromIntegral val / 1000.0 :: Double) ++ " pts"

  putStrLn "=== Single letter leaves ==="
  testLeave "?"        -- blank
  testLeave "A"
  testLeave "E"
  testLeave "I"
  testLeave "N"
  testLeave "R"
  testLeave "S"
  testLeave "T"
  testLeave "Q"
  testLeave "V"
  testLeave "Z"

  putStrLn ""
  putStrLn "=== 3-letter leaves ==="
  testLeave "ERS"      -- good
  testLeave "AIN"      -- okay
  testLeave "QVW"      -- terrible
  testLeave "ZZV"      -- terrible (if valid)
  testLeave "UUV"      -- bad vowels

  putStrLn ""
  putStrLn "=== 6-letter leaves ==="
  testLeave "AEINRS"   -- great for bingos
  testLeave "SATIRE"   -- one away from bingo
  testLeave "QWUUVV"   -- terrible
  testLeave "AEIIOU"   -- too many vowels
  testLeave "BCDFGH"   -- all consonants
