{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Magpie.KLV
import Magpie.Types
import Magpie.LetterDistribution

import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32)
import Data.Bits ((.&.), shiftR, testBit)

main :: IO ()
main = do
  klv <- loadKLV "data/lexica/CSW21.klv2"
  let ld = defaultEnglishLD

  putStrLn "KLV debugging:"
  putStrLn ""

  -- Print some word counts
  let counts = klvWordCounts klv
      nodes = klvKWGNodes klv

  putStrLn $ "Total nodes: " ++ show (VU.length nodes)
  putStrLn $ "Total leaves: " ++ show (VU.length (klvLeaveValues klv))
  putStrLn ""

  -- DAWG root
  let dawgRoot = nodeArcIndex (nodes VU.! 0)
  putStrLn $ "DAWG root: " ++ show dawgRoot
  putStrLn ""

  -- Print word counts for first nodes
  putStrLn "Word counts at root children:"
  let printNode i = do
        let node = nodes VU.! i
            t = nodeTile node
            c = counts VU.! i
        putStrLn $ "  node[" ++ show i ++ "]: tile=" ++ show t ++ " count=" ++ show c

  mapM_ printNode [fromIntegral dawgRoot .. fromIntegral dawgRoot + 10]

  -- Now trace the index for 'S'
  putStrLn ""
  putStrLn "Tracing index for 'S' (tile 19):"

  let traceS = go 0 (fromIntegral dawgRoot)
        where
          go !idx !nodeIdx
            | nodeIdx >= VU.length nodes = putStrLn "Out of bounds!"
            | otherwise = do
                let node = nodes VU.! nodeIdx
                    t = nodeTile node
                    c = counts VU.! nodeIdx
                    isEnd = nodeIsEnd node
                putStrLn $ "  At node " ++ show nodeIdx ++ ": tile=" ++ show t ++ " count=" ++ show c
                if t == 19  -- S
                  then putStrLn $ "  FOUND! Index = " ++ show idx
                  else if isEnd
                    then putStrLn "  End of siblings!"
                    else do
                      let nextCount = counts VU.! (nodeIdx + 1)
                          delta = c - nextCount
                      putStrLn $ "    delta = " ++ show c ++ " - " ++ show nextCount ++ " = " ++ show delta
                      go (idx + delta) (nodeIdx + 1)
  traceS

  where
    nodeArcIndex node = node .&. 0x3FFFFF
    nodeTile node = node `shiftR` 24
    nodeIsEnd node = testBit node 22
