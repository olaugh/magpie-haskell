{-# LANGUAGE OverloadedStrings #-}
module Main where

import Magpie.Types
import Magpie.KWG
import Magpie.KLV
import Magpie.Board
import Magpie.LetterDistribution
import Magpie.MoveGen

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

main :: IO ()
main = do
  kwg <- loadKWG "data/lexica/CSW21.kwg"
  klv <- loadKLV "data/lexica/CSW21.klv2"
  let ld = defaultEnglishLD

  putStrLn "Testing equity computation for rack AEINRST on empty board:"
  putStrLn ""

  let board = standardBoard
  case ldFromString ld "AEINRST" of
    Nothing -> putStrLn "Invalid rack"
    Just mls -> do
      let rack = rackFromList (ldSize ld) mls

      -- Generate all moves
      let allMoves = generateMoves kwg ld board rack
          top20 = take 20 allMoves

      -- Compute equity for each
      let addEquity m =
            let tiles = moveTiles m
                leave = computeLeave rack tiles
                leaveVal = klvGetLeaveValue klv leave
                equity = fromIntegral (moveScore m) + fromIntegral leaveVal / 1000.0
            in (m, leave, leaveVal, equity)

          movesWithEquity = map addEquity top20

      putStrLn "Top 20 moves by score, with equity:"
      putStrLn ""
      mapM_ printMoveEquity movesWithEquity

      putStrLn ""
      putStrLn "Now sorted by equity:"
      putStrLn ""
      let sortedByEquity = sortBy (comparing (\(_, _, _, eq) -> Down eq)) movesWithEquity
      mapM_ printMoveEquity (take 20 sortedByEquity)

      -- Test generateBestMove (use 100 for bagCount - full bag)
      putStrLn ""
      putStrLn "Best move by score (no KLV):"
      let bestNoKlv = generateBestMove defaultMoveGenConfig Nothing kwg ld board rack 100
      putStrLn $ "  " ++ showMove ld bestNoKlv ++ " score=" ++ show (moveScore bestNoKlv)

      putStrLn ""
      putStrLn "Best move by equity (with KLV):"
      let bestWithKlv = generateBestMove defaultMoveGenConfig (Just klv) kwg ld board rack 100
      putStrLn $ "  " ++ showMove ld bestWithKlv ++ " score=" ++ show (moveScore bestWithKlv) ++
                 " equity=" ++ show (moveEquity bestWithKlv)
  where
    printMoveEquity (m, leave, leaveVal, equity) = do
      let ld = defaultEnglishLD
      putStrLn $ "  " ++ showMove ld m ++
                 " | score=" ++ show (moveScore m) ++
                 " | leave=" ++ showLeave ld leave ++
                 " (" ++ show (fromIntegral leaveVal / 1000.0 :: Double) ++ " pts)" ++
                 " | equity=" ++ show equity

    showMove ld m =
      case moveType m of
        Pass -> "PASS"
        Exchange -> "EXCH " ++ ldToString ld (moveTiles m)
        TilePlacement ->
          let pos = [toEnum (fromEnum 'A' + moveCol m)] ++ show (moveRow m + 1)
              tiles = ldToString ld (moveTiles m)
              dirStr = case moveDir m of
                         Horizontal -> ""
                         Vertical -> "v"
          in pos ++ dirStr ++ " " ++ tiles ++ " (" ++ show (length $ moveTiles m) ++ " tiles)"

    showLeave ld r =
      let mls = rackToList r
      in if null mls then "(empty)" else ldToString ld mls
