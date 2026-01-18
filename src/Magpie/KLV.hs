{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | KLV (Known Leave Values) for leave lookup
--
-- KLV files contain:
-- - A small KWG (DAWG) for indexing rack subsets
-- - Leave values for each indexed subset
--
-- The KLV format (from wolges):
-- - uint32_t kwg_size (little-endian)
-- - uint32_t kwg_nodes[kwg_size]
-- - uint32_t number_of_leaves
-- - float leave_values[number_of_leaves]
module Magpie.KLV
  ( KLV(..)
  , LeaveValue
  , loadKLV
  , klvGetLeaveValue
  , klvGetLeaveValueFromTiles
  , klvGetWordIndex
  , klvUnfoundIndex
  ) where

import Magpie.Types (MachineLetter(..), Rack(..), isBlank)
import Control.Monad (forM_)

import qualified Data.ByteString as BS
import Data.Word (Word32, Word8)
import Data.Bits ((.&.), shiftR, testBit)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Int (Int32)
import Control.Monad.ST (runST, ST)

-- | Leave value stored as fixed-point Int32 (1000x resolution)
type LeaveValue = Int32

-- | KLV data structure
data KLV = KLV
  { klvKWGNodes     :: !(VU.Vector Word32)     -- ^ KWG nodes for leave lookup
  , klvWordCounts   :: !(VU.Vector Word32)     -- ^ Word counts at each node
  , klvLeaveValues  :: !(VU.Vector LeaveValue) -- ^ Leave values indexed by word index
  , klvName         :: !String                 -- ^ Name of this KLV
  } deriving (Show)

-- | Sentinel for unfound leave index
klvUnfoundIndex :: Word32
klvUnfoundIndex = 0xFFFFFFFF

-- | Load a KLV file
loadKLV :: FilePath -> IO KLV
loadKLV path = do
  bs <- BS.readFile path
  let bytes = BS.unpack bs
      -- Read kwg_size (first 4 bytes, little-endian)
      (kwgSizeBytes, rest1) = splitAt 4 bytes
      kwgSize = bytesToWord32LE kwgSizeBytes
      -- Read KWG nodes
      (kwgBytes, rest2) = splitAt (fromIntegral kwgSize * 4) rest1
      kwgNodes = VU.fromList $ bytesToWord32s kwgBytes
      -- Read number_of_leaves
      (numLeavesBytes, rest3) = splitAt 4 rest2
      numLeaves = bytesToWord32LE numLeavesBytes
      -- Read leave values (floats)
      (leaveBytes, _) = splitAt (fromIntegral numLeaves * 4) rest3
      leaveFloats = bytesToFloats leaveBytes
      -- Convert floats to fixed-point Int32 (1000x resolution)
      toInt32 :: Float -> Int32
      toInt32 f = round (f * 1000)
      leaveValues = VU.fromList $ map toInt32 leaveFloats
      -- Compute word counts
      wordCounts = computeWordCounts kwgNodes

  return $ KLV
    { klvKWGNodes = kwgNodes
    , klvWordCounts = wordCounts
    , klvLeaveValues = leaveValues
    , klvName = path
    }

-- | Convert 4 bytes to Word32 (little-endian)
bytesToWord32LE :: [Word8] -> Word32
bytesToWord32LE [a, b, c, d] =
  fromIntegral a + fromIntegral b * 256 +
  fromIntegral c * 65536 + fromIntegral d * 16777216
bytesToWord32LE _ = 0

-- | Convert bytes to Word32s (little-endian)
bytesToWord32s :: [Word8] -> [Word32]
bytesToWord32s [] = []
bytesToWord32s (a:b:c:d:rest) =
  let w = fromIntegral a + fromIntegral b * 256 +
          fromIntegral c * 65536 + fromIntegral d * 16777216
  in w : bytesToWord32s rest
bytesToWord32s _ = []

-- | Convert bytes to floats (little-endian IEEE 754)
bytesToFloats :: [Word8] -> [Float]
bytesToFloats [] = []
bytesToFloats (a:b:c:d:rest) =
  let w = fromIntegral a + fromIntegral b * 256 +
          fromIntegral c * 65536 + fromIntegral d * 16777216 :: Word32
  in word32ToFloat w : bytesToFloats rest
bytesToFloats _ = []

-- | Convert Word32 to Float (IEEE 754 bit pattern)
word32ToFloat :: Word32 -> Float
word32ToFloat w =
  let sign = if testBit w 31 then -1 else 1
      exp' = fromIntegral ((w `shiftR` 23) .&. 0xFF) - 127 :: Int
      mantissa = fromIntegral (w .&. 0x7FFFFF) / 8388608.0 + 1.0
  in if exp' == -127
     then 0.0  -- Denormalized or zero
     else sign * mantissa * (2.0 ** fromIntegral exp')

-- | Compute word counts for each node in the KWG
-- This is used for efficient leave index computation
-- Uses recursive memoization to handle forward arcs correctly
computeWordCounts :: VU.Vector Word32 -> VU.Vector Word32
computeWordCounts nodes = runST go
  where
    go :: forall s. ST s (VU.Vector Word32)
    go = do
      let n = VU.length nodes
      counts <- MVU.replicate n (0 :: Word32)
      -- 0 = unvisited, maxBound = visiting (cycle detection), other = computed

      let countAt :: Int -> ST s Word32
          countAt i
            | i < 0 || i >= n = return 0
            | otherwise = do
                c <- MVU.read counts i
                if c /= 0
                  then return (if c == maxBound then 0 else c)  -- Already computed or visiting
                  else do
                    -- Mark as visiting
                    MVU.write counts i maxBound

                    let node = nodes VU.! i
                        arcIdx = nodeArcIndex node
                        isEnd = nodeIsEnd node
                        accepts = nodeAccepts node

                    -- Get child count (follow arc)
                    childCount <- if arcIdx /= 0
                                  then countAt (fromIntegral arcIdx)
                                  else return 0

                    -- Get sibling count (next node if not end)
                    siblingCount <- if not isEnd
                                    then countAt (i + 1)
                                    else return 0

                    let thisCount = (if accepts then 1 else 0) + childCount + siblingCount

                    MVU.write counts i thisCount
                    return thisCount

      -- Compute counts for all nodes starting from the beginning
      mapM_ countAt [0 .. n - 1]

      VU.freeze counts

-- | Extract arc index from a KWG node
nodeArcIndex :: Word32 -> Word32
nodeArcIndex node = node .&. 0x3FFFFF

-- | Extract tile from a KWG node
nodeTile :: Word32 -> Word32
nodeTile node = node `shiftR` 24

-- | Check if node is end of sibling list
nodeIsEnd :: Word32 -> Bool
nodeIsEnd node = testBit node 22

-- | Check if node accepts (completes a word)
nodeAccepts :: Word32 -> Bool
nodeAccepts node = testBit node 23

-- | Get the DAWG root node index
klvDawgRoot :: KLV -> Word32
klvDawgRoot klv = nodeArcIndex (klvKWGNodes klv VU.! 0)

-- | Get leave value for a rack (returns fixed-point Int32, 1000x resolution)
klvGetLeaveValue :: KLV -> Rack -> LeaveValue
klvGetLeaveValue klv rack
  | rackIsEmpty rack = 0
  | otherwise =
      let idx = klvGetWordIndex klv rack
      in if idx == klvUnfoundIndex
         then 0
         else klvLeaveValues klv VU.! fromIntegral idx

-- | Get leave value directly from original rack and tiles played
-- This avoids allocating intermediate Rack structures
{-# INLINE klvGetLeaveValueFromTiles #-}
klvGetLeaveValueFromTiles :: KLV -> Rack -> [MachineLetter] -> LeaveValue
klvGetLeaveValueFromTiles klv originalRack tilesPlayed
  | leaveTotal == 0 = 0
  | otherwise = runST $ do
      -- Copy original rack counts to mutable vector
      leaveCounts <- VU.thaw (rackCounts originalRack)
      -- Remove played tiles
      forM_ tilesPlayed $ \ml -> do
        let idx = if isBlank ml
                  then 0  -- Blanked letters use blank from rack
                  else fromIntegral (unML ml)
        MVU.unsafeModify leaveCounts (subtract 1) idx
      -- Freeze and look up
      finalCounts <- VU.unsafeFreeze leaveCounts
      let leaveRack = Rack
            { rackCounts = finalCounts
            , rackDistSize = rackDistSize originalRack
            , rackTotal_ = leaveTotal
            }
          idx = klvGetWordIndexInternal klv leaveRack (klvDawgRoot klv)
      return $ if idx == klvUnfoundIndex
               then 0
               else klvLeaveValues klv VU.! fromIntegral idx
  where
    leaveTotal = rackTotal_ originalRack - length tilesPlayed

-- | Check if rack is empty
rackIsEmpty :: Rack -> Bool
rackIsEmpty rack = rackTotal_ rack == 0

-- | Get word index for a rack (for leave lookup)
klvGetWordIndex :: KLV -> Rack -> Word32
klvGetWordIndex klv rack
  | rackIsEmpty rack = klvUnfoundIndex
  | otherwise = klvGetWordIndexInternal klv rack (klvDawgRoot klv)

-- | Internal: traverse KWG to find word index for rack
-- Matches MAGPIE's klv_get_word_index_internal logic exactly
klvGetWordIndexInternal :: KLV -> Rack -> Word32 -> Word32
klvGetWordIndexInternal klv rack startNode = go 0 initLidx initCount startNode (rackTotal_ rack)
  where
    letters = rackCounts rack
    distSize = rackDistSize rack

    -- Advance lidx to next non-zero letter
    advanceLidx :: Int -> Int -> (Int, Int)
    advanceLidx lidx count
      | count > 0 = (lidx, count)
      | lidx + 1 >= distSize = (distSize, 0)
      | otherwise = advanceLidx (lidx + 1) (letters VU.! (lidx + 1))

    -- Initial lidx: find first non-zero letter
    (initLidx, initCount) = advanceLidx 0 (letters VU.! 0)

    go :: Word32 -> Int -> Int -> Word32 -> Int -> Word32
    go !idx !lidx !lidxCount !nodeIdx !remaining
      | nodeIdx == 0 = klvUnfoundIndex
      | lidx >= distSize = klvUnfoundIndex
      | otherwise =
          -- Traverse to the current letter
          let (newNodeIdx, newIdx) = incrementNodeToML klv nodeIdx idx (MachineLetter $ fromIntegral lidx)
          in if newNodeIdx == 0
             then klvUnfoundIndex
             else
               -- Decrement counts
               let newLidxCount = lidxCount - 1
                   newRemaining = remaining - 1
               in if newRemaining == 0
                  -- Done! Return the index immediately (before following arc)
                  then newIdx
                  else
                    -- Advance to next letter if needed
                    let (nextLidx, nextCount) =
                          if newLidxCount > 0
                          then (lidx, newLidxCount)
                          else advanceLidx (lidx + 1) (if lidx + 1 < distSize then letters VU.! (lidx + 1) else 0)
                        -- Follow arc to continue
                        (arcNodeIdx, arcIdx) = followArc klv newNodeIdx newIdx
                    in go arcIdx nextLidx nextCount arcNodeIdx newRemaining

-- | Increment node to a specific machine letter, updating word index
incrementNodeToML :: KLV -> Word32 -> Word32 -> MachineLetter -> (Word32, Word32)
incrementNodeToML klv nodeIdx wordIdx (MachineLetter ml)
  | nodeIdx == 0 = (0, klvUnfoundIndex)
  | otherwise = go nodeIdx wordIdx
  where
    mlW = fromIntegral ml
    nodes = klvKWGNodes klv
    counts = klvWordCounts klv

    go !i !wIdx =
      let node = nodes VU.! fromIntegral i
          tile = nodeTile node
      in if tile == mlW
         then (i, wIdx)
         else if nodeIsEnd node
              then (0, klvUnfoundIndex)
              else
                -- Move to next sibling, adjusting word index
                let siblingWordCount = counts VU.! fromIntegral i -
                                       counts VU.! fromIntegral (i + 1)
                in go (i + 1) (wIdx + siblingWordCount)

-- | Follow arc to child node, incrementing word index
followArc :: KLV -> Word32 -> Word32 -> (Word32, Word32)
followArc klv nodeIdx wordIdx
  | nodeIdx == 0 = (0, klvUnfoundIndex)
  | otherwise =
      let node = klvKWGNodes klv VU.! fromIntegral nodeIdx
          arcIdx = nodeArcIndex node
      in (arcIdx, wordIdx + 1)
