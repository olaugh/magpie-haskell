{-# LANGUAGE BangPatterns #-}

-- | LeaveMap for O(1) leave value lookup during move generation
--
-- The LeaveMap precomputes leave values for all 2^n subsets of a rack.
-- During move generation, as tiles are played, we track which tiles
-- remain using a bitmask, and look up the leave value in O(1).
--
-- Key insight: a 7-tile rack has 128 possible subsets. We compute
-- leave values for all 128 subsets once at the start, then lookup
-- is just an array index.
module Magpie.LeaveMap
  ( LeaveMap(..)
  , leaveMapCreate
  , leaveMapGetValue
  , leaveMapEmpty
  , maxRackSize
  ) where

import Magpie.Types (Rack(..), MachineLetter(..), rackFromList, emptyRack)
import Magpie.KLV (KLV, klvGetLeaveValue, LeaveValue)
import qualified Data.Vector.Unboxed as VU
import Data.Bits (shiftL, testBit, popCount)

-- | Maximum rack size (7 tiles)
maxRackSize :: Int
maxRackSize = 7

-- | LeaveMap for efficient leave lookup
data LeaveMap = LeaveMap
  { lmLeaveValues :: !(VU.Vector LeaveValue)  -- ^ Leave values indexed by subset mask
  , lmRackTiles   :: ![MachineLetter]         -- ^ Tiles in order (index -> tile)
  , lmDistSize    :: !Int                     -- ^ Distribution size for rack creation
  } deriving (Show)

-- | Empty leave map (returns 0 for all lookups)
leaveMapEmpty :: Int -> LeaveMap
leaveMapEmpty distSize = LeaveMap
  { lmLeaveValues = VU.replicate (1 `shiftL` maxRackSize) 0
  , lmRackTiles = []
  , lmDistSize = distSize
  }

-- | Create a LeaveMap for a rack, precomputing all subset leave values
--
-- The rack tiles are assigned indices 0 to n-1 in order.
-- For each subset mask (0 to 2^n - 1), we compute the leave value
-- of the tiles corresponding to set bits.
leaveMapCreate :: KLV -> Rack -> LeaveMap
leaveMapCreate klv rack =
  let -- Convert rack to ordered list of tiles
      tiles = rackToOrderedList rack
      numTiles = length tiles
      distSize = rackDistSize rack

      -- Compute leave value for each subset
      numSubsets = 1 `shiftL` numTiles
      leaveValues = VU.generate numSubsets $ \mask ->
        let subsetTiles = [tiles !! i | i <- [0..numTiles-1], testBit mask i]
            subsetRack = rackFromList distSize subsetTiles
        in klvGetLeaveValue klv subsetRack

  in LeaveMap
     { lmLeaveValues = leaveValues
     , lmRackTiles = tiles
     , lmDistSize = distSize
     }

-- | Get leave value for a subset mask
-- The mask has bit i set if tile i is still in the leave
leaveMapGetValue :: LeaveMap -> Int -> LeaveValue
leaveMapGetValue lm mask
  | mask < VU.length (lmLeaveValues lm) = lmLeaveValues lm VU.! mask
  | otherwise = 0

-- | Convert rack to ordered list of tiles
-- Each tile type appears count times consecutively
rackToOrderedList :: Rack -> [MachineLetter]
rackToOrderedList rack =
  [ MachineLetter (fromIntegral i)
  | i <- [0 .. VU.length (rackCounts rack) - 1]
  , _ <- [1 .. rackCounts rack VU.! i]
  ]
