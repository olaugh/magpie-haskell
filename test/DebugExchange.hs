{-# LANGUAGE OverloadedStrings #-}
module Main where

import Magpie.KLV
import Magpie.Types
import Magpie.LetterDistribution
import Magpie.MoveGen (generateExchanges, computeLeave)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

main :: IO ()
main = do
  klv <- loadKLV "data/lexica/CSW21.klv2"
  let ld = defaultEnglishLD

  putStrLn "Exchange equity debug:"
  putStrLn ""

  -- First, test if 7-tile racks have non-zero leave values
  putStrLn "=== Testing 7-tile rack leave values ==="
  let testRack7 str = case ldFromString ld str of
        Nothing -> putStrLn $ str ++ ": invalid"
        Just mls -> do
          let rack = rackFromList (ldSize ld) mls
              idx = klvGetWordIndex klv rack
              val = klvGetLeaveValue klv rack
          putStrLn $ str ++ ": idx=" ++ show idx ++ " val=" ++ show (fromIntegral val / 1000.0 :: Double) ++ " pts"

  testRack7 "QWUUVVX"  -- bad
  testRack7 "AEEINRS"  -- good
  testRack7 "SATIREN"  -- one-away from bingo
  testRack7 "AEIURST"  -- SATIRE + U
  testRack7 "BCDFGHJ"  -- all consonants
  putStrLn ""

  -- Test with a bad rack: QWUUVVX
  case ldFromString ld "QWUUVVX" of
    Nothing -> putStrLn "Invalid rack"
    Just mls -> do
      let rack = rackFromList (ldSize ld) mls
          passLeave = rack  -- pass keeps entire rack
          passLeaveVal = klvGetLeaveValue klv passLeave

      putStrLn $ "Rack: QWUUVVX"
      putStrLn $ "Pass leave value: " ++ show (fromIntegral passLeaveVal / 1000.0 :: Double) ++ " pts"
      putStrLn ""

      -- Generate all exchange moves and evaluate them
      let exchanges = generateExchanges rack
      putStrLn $ "Number of exchange moves: " ++ show (length exchanges)
      putStrLn ""

      -- Show top 10 best exchanges (properly sorted)
      let evaluatedExchanges =
            [ (klvGetLeaveValue klv (computeLeave rack (moveTiles ex)), moveTiles ex, computeLeave rack (moveTiles ex))
            | ex <- exchanges
            ]
          sorted = take 10 $ sortBy (comparing (Down . (\(v,_,_) -> v))) evaluatedExchanges

      putStrLn "Top 10 exchanges by leave value:"
      mapM_ (\(val, tiles, leave) -> do
        putStrLn $ "  Exchange " ++ ldToString ld tiles ++
                   " -> keep " ++ show (rackTotal leave) ++ " tiles" ++
                   ", leave=" ++ show (fromIntegral val / 1000.0 :: Double) ++ " pts"
        ) sorted

      putStrLn ""
      putStrLn $ "Best exchange better than pass? " ++
                 show (case sorted of
                         ((val,_,_):_) -> val > passLeaveVal
                         [] -> False)

  putStrLn ""
  putStrLn "=== Testing a more normal rack ==="

  -- Test with a more normal bad rack: AAIOQUU
  case ldFromString ld "AAIOQUU" of
    Nothing -> putStrLn "Invalid rack"
    Just mls -> do
      let rack = rackFromList (ldSize ld) mls
          passLeave = rack
          passLeaveVal = klvGetLeaveValue klv passLeave

      putStrLn $ "Rack: AAIOQUU"
      putStrLn $ "Pass leave value: " ++ show (fromIntegral passLeaveVal / 1000.0 :: Double) ++ " pts"
      putStrLn ""

      let exchanges = generateExchanges rack
          evaluatedExchanges =
            [ (klvGetLeaveValue klv (computeLeave rack (moveTiles ex)), moveTiles ex, computeLeave rack (moveTiles ex))
            | ex <- exchanges
            ]
          sorted = take 10 $ sortBy (comparing (Down . (\(v,_,_) -> v))) evaluatedExchanges

      putStrLn "Top 10 exchanges by leave value:"
      mapM_ (\(val, tiles, leave) -> do
        putStrLn $ "  Exchange " ++ ldToString ld tiles ++
                   " -> keep " ++ show (rackTotal leave) ++ " tiles" ++
                   ", leave=" ++ show (fromIntegral val / 1000.0 :: Double) ++ " pts"
        ) sorted

      putStrLn ""
      putStrLn $ "Best exchange better than pass? " ++
                 show (case sorted of
                         ((val,_,_):_) -> val > passLeaveVal
                         [] -> False)
