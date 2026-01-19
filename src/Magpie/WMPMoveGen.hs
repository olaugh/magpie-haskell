{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | WMP-based move generation
-- Ported from MAGPIE's wmp_move_gen.h
--
-- This module provides efficient move generation using the Word Map (WMP)
-- data structure. It enumerates all subracks of the player's rack, checks
-- which form valid words via WMP lookup, and tracks playthrough tiles.
--
-- For performance, playthrough state uses mutable ST-based operations
-- to avoid repeated allocation during shadow traversal.
module Magpie.WMPMoveGen
  ( -- * Types
    WMPMoveGen(..)
  , SubrackInfo(..)
  , WMPStatic(..)
  , MWMPPlaythrough

    -- * Initialization
  , wmpMoveGenInit
  , wmpMoveGenIsActive
  , wmpStaticFromMoveGen
  , newMWMPPlaythrough

    -- * Immutable playthrough management (legacy)
  , wmpMoveGenResetPlaythrough
  , wmpMoveGenHasPlaythrough
  , wmpMoveGenAddPlaythroughLetter
  , wmpMoveGenSavePlaythroughState
  , wmpMoveGenRestorePlaythroughState
  , wmpMoveGenIncrementPlaythroughBlocks

    -- * Mutable playthrough management (ST-based)
  , mwmpReset
  , mwmpHasPlaythrough
  , mwmpAddLetter
  , mwmpSave
  , mwmpRestore
  , mwmpIncrementBlocks
  , mwmpCheckBingoExistence
  , mwmpGetPlaythroughBitRack
  , mwmpGetNumTilesPlayedThrough

    -- * Word existence checking
  , wmpMoveGenCheckNonplaythroughExistence
  , wmpMoveGenNonplaythroughWordOfLengthExists
  , wmpMoveGenGetNonplaythroughBestLeaveValues
  , wmpMoveGenCheckPlaythroughFullRackExistence
  , wmpStaticWordOfLengthExists
  , wmpStaticGetBestLeaveValue

    -- * Subrack iteration
  , wmpMoveGenPlaythroughSubracksInit
  , wmpMoveGenGetNumSubrackCombinations
  , wmpMoveGenGetSubrackWords
  , wmpMoveGenGetWord
  , wmpMoveGenGetLeaveValue

    -- * Best leave computation
  , wmpMoveGenComputeBestLeavesWithKLV

    -- * Constants
  , minimumWordLength
  , rackSize
  ) where

import Magpie.BitRack
import Magpie.WMP
import Magpie.Equity (Equity(..))
import Magpie.Types (MachineLetter(..), Rack(..), rackCounts, rackTotal_)

import Data.Bits (shiftL)
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- | Constants
minimumWordLength :: Int
minimumWordLength = 2

rackSize :: Int
rackSize = 7

-- | Combination offsets for subrack enumeration (Pascal's triangle row 7)
-- C(7,0)=1, C(7,1)=7, C(7,2)=21, C(7,3)=35, C(7,4)=35, C(7,5)=21, C(7,6)=7, C(7,7)=1
-- Offsets: 0, 1, 8, 29, 64, 99, 120, 127
combinationOffsets :: VU.Vector Int
combinationOffsets = VU.fromList [0, 1, 8, 29, 64, 99, 120, 127, 128]

getCombinationOffset :: Int -> Int
getCombinationOffset size
  | size < 0 || size > rackSize = 0
  | otherwise = combinationOffsets VU.! size

-- | Information about a subrack
data SubrackInfo = SubrackInfo
  { siSubrack    :: !BitRack
  , siWMPEntry   :: !(Maybe WMPEntry)
  , siLeaveValue :: !Equity
  } deriving (Show)

emptySubrackInfo :: SubrackInfo
emptySubrackInfo = SubrackInfo emptyBitRack Nothing (Equity 0)

-- | WMP move generator state
data WMPMoveGen = WMPMoveGen
  { wmgWMP                  :: !(Maybe WMP)
  , wmgPlayerBitRack        :: !BitRack
  , wmgFullRackSize         :: !Int

    -- Playthrough state
  , wmgPlaythroughBitRack   :: !BitRack
  , wmgNumTilesPlayedThrough :: !Int
  , wmgPlaythroughBlocks    :: !Int

    -- Saved playthrough state for restore
  , wmgPlaythroughBitRackCopy    :: !BitRack
  , wmgNumTilesPlayedThroughCopy :: !Int
  , wmgPlaythroughBlocksCopy     :: !Int

    -- Subrack enumeration results
  , wmgNonplaythroughInfos      :: !(V.Vector SubrackInfo)
  , wmgPlaythroughInfos         :: !(V.Vector SubrackInfo)
  , wmgNonplaythroughBestLeaves :: !(VU.Vector Equity)
  , wmgNonplaythroughHasWord    :: !(VU.Vector Bool)
  , wmgCountBySize              :: !(VU.Vector Int)

    -- Current query state
  , wmgTilesToPlay  :: !Int
  , wmgWordLength   :: !Int
  , wmgBuffer       :: ![[MachineLetter]]
  , wmgLeaveValue   :: !Equity
  } deriving (Show)

-- | Create an inactive WMP move generator
wmpMoveGenEmpty :: WMPMoveGen
wmpMoveGenEmpty = WMPMoveGen
  { wmgWMP = Nothing
  , wmgPlayerBitRack = emptyBitRack
  , wmgFullRackSize = 0
  , wmgPlaythroughBitRack = emptyBitRack
  , wmgNumTilesPlayedThrough = 0
  , wmgPlaythroughBlocks = 0
  , wmgPlaythroughBitRackCopy = emptyBitRack
  , wmgNumTilesPlayedThroughCopy = 0
  , wmgPlaythroughBlocksCopy = 0
  , wmgNonplaythroughInfos = V.replicate 128 emptySubrackInfo
  , wmgPlaythroughInfos = V.replicate 128 emptySubrackInfo
  , wmgNonplaythroughBestLeaves = VU.replicate (rackSize + 1) (Equity minBound)
  , wmgNonplaythroughHasWord = VU.replicate (rackSize + 1) False
  , wmgCountBySize = VU.replicate (rackSize + 1) 0
  , wmgTilesToPlay = 0
  , wmgWordLength = 0
  , wmgBuffer = []
  , wmgLeaveValue = Equity 0
  }

-- | Initialize WMP move generator
wmpMoveGenInit :: Maybe WMP -> Rack -> WMPMoveGen
wmpMoveGenInit Nothing _ = wmpMoveGenEmpty
wmpMoveGenInit (Just wmp) rack =
  let bitRack = rackToBitRack rack
      fullSize = rackTotal_ rack
  in wmpMoveGenEmpty
     { wmgWMP = Just wmp
     , wmgPlayerBitRack = bitRack
     , wmgFullRackSize = fullSize
     }

-- | Convert Rack to BitRack (matches C bit_rack_create_from_rack)
rackToBitRack :: Rack -> BitRack
rackToBitRack rack =
  let counts = rackCounts rack
      setLetter br ml =
        let count = counts VU.! ml
        in bitRackSetLetterCount (MachineLetter (fromIntegral ml)) count br
  in foldl setLetter emptyBitRack [0 .. VU.length counts - 1]

-- | Check if WMP move gen is active
wmpMoveGenIsActive :: WMPMoveGen -> Bool
wmpMoveGenIsActive wmg = case wmgWMP wmg of
  Nothing -> False
  Just _  -> True

-- | Reset playthrough state
wmpMoveGenResetPlaythrough :: WMPMoveGen -> WMPMoveGen
wmpMoveGenResetPlaythrough wmg = wmg
  { wmgPlaythroughBitRack = emptyBitRack
  , wmgNumTilesPlayedThrough = 0
  , wmgPlaythroughBlocks = 0
  }

-- | Check if there are playthrough tiles
wmpMoveGenHasPlaythrough :: WMPMoveGen -> Bool
wmpMoveGenHasPlaythrough wmg = wmgNumTilesPlayedThrough wmg > 0

-- | Add a playthrough letter
wmpMoveGenAddPlaythroughLetter :: MachineLetter -> WMPMoveGen -> WMPMoveGen
wmpMoveGenAddPlaythroughLetter ml wmg = wmg
  { wmgPlaythroughBitRack = bitRackAddLetter ml (wmgPlaythroughBitRack wmg)
  , wmgNumTilesPlayedThrough = wmgNumTilesPlayedThrough wmg + 1
  }

-- | Increment playthrough blocks
wmpMoveGenIncrementPlaythroughBlocks :: WMPMoveGen -> WMPMoveGen
wmpMoveGenIncrementPlaythroughBlocks wmg = wmg
  { wmgPlaythroughBlocks = wmgPlaythroughBlocks wmg + 1 }

-- | Save playthrough state
wmpMoveGenSavePlaythroughState :: WMPMoveGen -> WMPMoveGen
wmpMoveGenSavePlaythroughState wmg = wmg
  { wmgPlaythroughBitRackCopy = wmgPlaythroughBitRack wmg
  , wmgNumTilesPlayedThroughCopy = wmgNumTilesPlayedThrough wmg
  , wmgPlaythroughBlocksCopy = wmgPlaythroughBlocks wmg
  }

-- | Restore playthrough state
wmpMoveGenRestorePlaythroughState :: WMPMoveGen -> WMPMoveGen
wmpMoveGenRestorePlaythroughState wmg = wmg
  { wmgPlaythroughBitRack = wmgPlaythroughBitRackCopy wmg
  , wmgNumTilesPlayedThrough = wmgNumTilesPlayedThroughCopy wmg
  , wmgPlaythroughBlocks = wmgPlaythroughBlocksCopy wmg
  }

-- | Enumerate nonplaythrough subracks and check word existence
-- Returns updated WMPMoveGen with subrack info and word existence flags
wmpMoveGenCheckNonplaythroughExistence :: Bool -> (Int -> Equity) -> WMPMoveGen -> WMPMoveGen
wmpMoveGenCheckNonplaythroughExistence checkLeaves getLeaveValue wmg =
  case wmgWMP wmg of
    Nothing -> wmg
    Just wmp -> runST $ do
      -- Mutable arrays for enumeration
      infosRef <- V.thaw (wmgNonplaythroughInfos wmg)
      countBySize <- VUM.replicate (rackSize + 1) (0 :: Int)
      hasWord <- VUM.replicate (rackSize + 1) False
      bestLeaves <- VUM.replicate (rackSize + 1) (Equity minBound)

      -- Enumerate subracks recursively
      let playerBitRack = wmgPlayerBitRack wmg
          fullRackSize = wmgFullRackSize wmg

          -- No explicit type signature to avoid ST type variable issues
          enumerate !current !nextMl !count !leaveIdx = do
            -- Find next letter with non-zero count
            let findNext ml
                  | ml >= 32 = Nothing
                  | bitRackGetLetterCount (MachineLetter (fromIntegral ml)) playerBitRack > 0 = Just ml
                  | otherwise = findNext (ml + 1)

            case findNext nextMl of
              Nothing -> do
                -- Base case: record this subrack
                let offset = getCombinationOffset count
                cnt <- VUM.read countBySize count
                let insertIdx = offset + cnt
                let leaveVal = getLeaveValue leaveIdx
                VM.write infosRef insertIdx (SubrackInfo current Nothing leaveVal)
                VUM.write countBySize count (cnt + 1)

              Just ml -> do
                let maxNum = bitRackGetLetterCount (MachineLetter (fromIntegral ml)) playerBitRack
                    mlLetter = MachineLetter (fromIntegral ml)

                -- For each count 0..maxNum of this letter in the subrack
                forM_ [0..maxNum] $ \i -> do
                  let current' = iterate (bitRackAddLetter mlLetter) current !! i
                      -- Update leave index for complement tracking
                      leaveIdx' = leaveIdx  -- Simplified: full leave tracking would need more
                  enumerate current' (ml + 1) (count + i) leaveIdx'

      enumerate emptyBitRack (0 :: Int) 0 ((1 `shiftL` fullRackSize) - 1)

      -- Check word existence for each size
      forM_ [minimumWordLength .. fullRackSize] $ \size -> do
        let offset = getCombinationOffset size
        cnt <- VUM.read countBySize size
        let leaveSize = fullRackSize - size
        when checkLeaves $ VUM.write bestLeaves leaveSize (Equity minBound)

        forM_ [0 .. cnt - 1] $ \idxForSize -> do
          let idx = offset + idxForSize
          info <- VM.read infosRef idx
          let entry = wmpGetWordEntry wmp (siSubrack info) size
          VM.write infosRef idx (info { siWMPEntry = entry })

          when (isJust entry) $ do
            VUM.write hasWord size True
            when checkLeaves $ do
              bestVal <- VUM.read bestLeaves leaveSize
              when (siLeaveValue info > bestVal) $
                VUM.write bestLeaves leaveSize (siLeaveValue info)

      -- Freeze and return
      infos' <- V.freeze infosRef
      countBySize' <- VU.freeze countBySize
      hasWord' <- VU.freeze hasWord
      bestLeaves' <- VU.freeze bestLeaves

      return wmg
        { wmgNonplaythroughInfos = infos'
        , wmgCountBySize = countBySize'
        , wmgNonplaythroughHasWord = hasWord'
        , wmgNonplaythroughBestLeaves = bestLeaves'
        }

-- | Check if nonplaythrough word of given length exists
wmpMoveGenNonplaythroughWordOfLengthExists :: Int -> WMPMoveGen -> Bool
wmpMoveGenNonplaythroughWordOfLengthExists len wmg
  | len < 0 || len > rackSize = False
  | otherwise = wmgNonplaythroughHasWord wmg VU.! len

-- | Get best leave values for each leave size
wmpMoveGenGetNonplaythroughBestLeaveValues :: WMPMoveGen -> VU.Vector Equity
wmpMoveGenGetNonplaythroughBestLeaveValues = wmgNonplaythroughBestLeaves

-- | Check if full rack bingo exists with playthrough tiles
wmpMoveGenCheckPlaythroughFullRackExistence :: WMPMoveGen -> Bool
wmpMoveGenCheckPlaythroughFullRackExistence wmg =
  case wmgWMP wmg of
    Nothing -> False
    Just wmp ->
      let fullSize = wmgFullRackSize wmg
          subrack = bitRackAddBitRack (wmgPlayerBitRack wmg) (wmgPlaythroughBitRack wmg)
          wordSize = fullSize + wmgNumTilesPlayedThrough wmg
          entry = wmpGetWordEntry wmp subrack wordSize
      in isJust entry

-- | Helper to add two BitRacks
bitRackAddBitRack :: BitRack -> BitRack -> BitRack
bitRackAddBitRack (BitRack l1 h1) (BitRack l2 h2) = BitRack (l1 + l2) (h1 + h2)

-- | Initialize playthrough subracks for an anchor
wmpMoveGenPlaythroughSubracksInit :: Int -> Int -> WMPMoveGen -> WMPMoveGen
wmpMoveGenPlaythroughSubracksInit tilesToPlay wordLength wmg =
  let numTilesPlayedThrough = wordLength - tilesToPlay
      wmg' = wmg
        { wmgWordLength = wordLength
        , wmgNumTilesPlayedThrough = numTilesPlayedThrough
        , wmgTilesToPlay = tilesToPlay
        }
  in if numTilesPlayedThrough == 0
       then wmg'  -- Use nonplaythrough subracks
       else
         -- Create playthrough subracks by adding playthrough BitRack
         let offset = getCombinationOffset tilesToPlay
             count = wmgCountBySize wmg VU.! tilesToPlay
             playthroughBR = wmgPlaythroughBitRack wmg
             playInfos = V.imap (\i si ->
               if i >= offset && i < offset + count
                 then si { siSubrack = bitRackAddBitRack (siSubrack si) playthroughBR }
                 else si) (wmgNonplaythroughInfos wmg)
         in wmg' { wmgPlaythroughInfos = playInfos }

-- | Get number of subrack combinations for current tiles to play
wmpMoveGenGetNumSubrackCombinations :: WMPMoveGen -> Int
wmpMoveGenGetNumSubrackCombinations wmg =
  wmgCountBySize wmg VU.! wmgTilesToPlay wmg

-- | Get words for a subrack at given index
-- Returns (updated wmg, list of words)
wmpMoveGenGetSubrackWords :: Int -> WMPMoveGen -> (WMPMoveGen, [[MachineLetter]])
wmpMoveGenGetSubrackWords idxForSize wmg =
  case wmgWMP wmg of
    Nothing -> (wmg, [])
    Just wmp ->
      let offset = getCombinationOffset (wmgTilesToPlay wmg)
          subrackIdx = offset + idxForSize
          isPlaythrough = wmgWordLength wmg > wmgTilesToPlay wmg

          -- Get the appropriate subrack info
          info = if isPlaythrough
                   then wmgPlaythroughInfos wmg V.! subrackIdx
                   else wmgNonplaythroughInfos wmg V.! subrackIdx

          -- For playthrough, we need to look up the entry
          entry = if isPlaythrough
                    then wmpGetWordEntry wmp (siSubrack info) (wmgWordLength wmg)
                    else siWMPEntry info

      in case entry of
           Nothing -> (wmg, [])
           Just _ ->
             let wordList = wmpWriteWordsToBuffer wmp (siSubrack info) (wmgWordLength wmg)
             in (wmg { wmgBuffer = wordList, wmgLeaveValue = siLeaveValue info }, wordList)

-- | Get word at index from buffer
wmpMoveGenGetWord :: Int -> WMPMoveGen -> [MachineLetter]
wmpMoveGenGetWord wordIdx wmg
  | wordIdx < 0 || wordIdx >= length (wmgBuffer wmg) = []
  | otherwise = wmgBuffer wmg !! wordIdx

-- | Get leave value for a subrack
wmpMoveGenGetLeaveValue :: Int -> WMPMoveGen -> Equity
wmpMoveGenGetLeaveValue subrackIdx wmg =
  let offset = getCombinationOffset (wmgTilesToPlay wmg)
      info = wmgNonplaythroughInfos wmg V.! (offset + subrackIdx)
  in siLeaveValue info

-- | Compute best leave values using KLV
-- This iterates through all subracks that form valid words and computes
-- the leave value (rack - subrack) using the KLV lookup function.
-- The getLeaveValue function should take a Rack and return the leave value.
wmpMoveGenComputeBestLeavesWithKLV :: (Rack -> Int32) -> Rack -> WMPMoveGen -> WMPMoveGen
wmpMoveGenComputeBestLeavesWithKLV getLeaveValue originalRack wmg =
  case wmgWMP wmg of
    Nothing -> wmg
    Just _ -> runST $ do
      bestLeaves <- VUM.replicate (rackSize + 1) (Equity minBound)

      let fullRackSize = wmgFullRackSize wmg
          playerBitRack = wmgPlayerBitRack wmg
          distSize = VU.length (rackCounts originalRack)

      -- Iterate through each word size
      forM_ [minimumWordLength .. fullRackSize] $ \wordSize -> do
        let offset = getCombinationOffset wordSize
            count = wmgCountBySize wmg VU.! wordSize
            leaveSize = fullRackSize - wordSize

        forM_ [0 .. count - 1] $ \idxForSize -> do
          let idx = offset + idxForSize
              info = wmgNonplaythroughInfos wmg V.! idx

          -- Only process subracks that form valid words
          when (isJust (siWMPEntry info)) $ do
            -- Compute the leave (rack - subrack)
            let subrack = siSubrack info
                leaveRack = bitRackSubtract playerBitRack subrack distSize originalRack
                leaveVal = Equity (getLeaveValue leaveRack)

            -- Update best leave if this is better
            currentBest <- VUM.read bestLeaves leaveSize
            when (leaveVal > currentBest) $
              VUM.write bestLeaves leaveSize leaveVal

      bestLeaves' <- VU.freeze bestLeaves
      return wmg { wmgNonplaythroughBestLeaves = bestLeaves' }
  where
    -- Compute leave = rack - subrack
    -- Returns a Rack with the remaining tiles
    bitRackSubtract :: BitRack -> BitRack -> Int -> Rack -> Rack
    bitRackSubtract _rackBR subrackBR distSize origRack =
      let newCounts = VU.generate distSize $ \i ->
            let ml = MachineLetter (fromIntegral i)
                origCount = rackCounts origRack VU.! i
                subCount = bitRackGetLetterCount ml subrackBR
            in origCount - subCount
          newTotal = VU.sum newCounts
      in Rack
         { rackCounts = newCounts
         , rackDistSize = distSize
         , rackTotal_ = newTotal
         }

-- ============================================================================
-- Static WMP state (immutable after initialization)
-- ============================================================================

-- | Static (immutable) parts of WMP state
-- This contains the WMP, rack info, and subrack enumeration results
data WMPStatic = WMPStatic
  { wsWMP                      :: !(Maybe WMP)
  , wsPlayerBitRack            :: !BitRack
  , wsFullRackSize             :: !Int
  , wsNonplaythroughInfos      :: !(V.Vector SubrackInfo)
  , wsNonplaythroughBestLeaves :: !(VU.Vector Equity)
  , wsNonplaythroughHasWord    :: !(VU.Vector Bool)
  , wsCountBySize              :: !(VU.Vector Int)
  } deriving (Show)

-- | Extract static parts from WMPMoveGen
wmpStaticFromMoveGen :: WMPMoveGen -> WMPStatic
wmpStaticFromMoveGen wmg = WMPStatic
  { wsWMP = wmgWMP wmg
  , wsPlayerBitRack = wmgPlayerBitRack wmg
  , wsFullRackSize = wmgFullRackSize wmg
  , wsNonplaythroughInfos = wmgNonplaythroughInfos wmg
  , wsNonplaythroughBestLeaves = wmgNonplaythroughBestLeaves wmg
  , wsNonplaythroughHasWord = wmgNonplaythroughHasWord wmg
  , wsCountBySize = wmgCountBySize wmg
  }

-- | Check if nonplaythrough word of given length exists (static version)
wmpStaticWordOfLengthExists :: Int -> WMPStatic -> Bool
wmpStaticWordOfLengthExists len ws
  | len < 0 || len > rackSize = False
  | otherwise = wsNonplaythroughHasWord ws VU.! len

-- | Get the best leave value for a given number of tiles played (static version)
-- Returns the best leave value among all valid subracks of that size.
-- The leave size is (full_rack_size - tiles_played).
wmpStaticGetBestLeaveValue :: Int -> WMPStatic -> Equity
wmpStaticGetBestLeaveValue tilesPlayed ws =
  let leaveSize = wsFullRackSize ws - tilesPlayed
  in if leaveSize < 0 || leaveSize > rackSize
     then 0
     else wsNonplaythroughBestLeaves ws VU.! leaveSize

-- ============================================================================
-- Mutable playthrough state (ST monad)
-- ============================================================================

-- | Mutable playthrough state for efficient shadow traversal
-- Uses STRef for O(1) mutation without allocation
data MWMPPlaythrough s = MWMPPlaythrough
  { mwpPlaythroughLo          :: !(STRef s Word64)  -- BitRack low
  , mwpPlaythroughHi          :: !(STRef s Word64)  -- BitRack high
  , mwpNumTilesPlayedThrough  :: !(STRef s Int)
  , mwpPlaythroughBlocks      :: !(STRef s Int)
    -- Saved state for restore
  , mwpPlaythroughLoCopy      :: !(STRef s Word64)
  , mwpPlaythroughHiCopy      :: !(STRef s Word64)
  , mwpNumTilesPlayedThroughCopy :: !(STRef s Int)
  , mwpPlaythroughBlocksCopy  :: !(STRef s Int)
  }

-- | Create a new mutable playthrough state
newMWMPPlaythrough :: ST s (MWMPPlaythrough s)
newMWMPPlaythrough = do
  lo <- newSTRef 0
  hi <- newSTRef 0
  num <- newSTRef 0
  blocks <- newSTRef 0
  loCopy <- newSTRef 0
  hiCopy <- newSTRef 0
  numCopy <- newSTRef 0
  blocksCopy <- newSTRef 0
  return MWMPPlaythrough
    { mwpPlaythroughLo = lo
    , mwpPlaythroughHi = hi
    , mwpNumTilesPlayedThrough = num
    , mwpPlaythroughBlocks = blocks
    , mwpPlaythroughLoCopy = loCopy
    , mwpPlaythroughHiCopy = hiCopy
    , mwpNumTilesPlayedThroughCopy = numCopy
    , mwpPlaythroughBlocksCopy = blocksCopy
    }

-- | Reset playthrough state
mwmpReset :: MWMPPlaythrough s -> ST s ()
mwmpReset mwp = do
  writeSTRef (mwpPlaythroughLo mwp) 0
  writeSTRef (mwpPlaythroughHi mwp) 0
  writeSTRef (mwpNumTilesPlayedThrough mwp) 0
  writeSTRef (mwpPlaythroughBlocks mwp) 0

-- | Check if there are playthrough tiles
mwmpHasPlaythrough :: MWMPPlaythrough s -> ST s Bool
mwmpHasPlaythrough mwp = do
  num <- readSTRef (mwpNumTilesPlayedThrough mwp)
  return (num > 0)

-- | Add a playthrough letter
mwmpAddLetter :: MachineLetter -> MWMPPlaythrough s -> ST s ()
mwmpAddLetter ml mwp = do
  lo <- readSTRef (mwpPlaythroughLo mwp)
  hi <- readSTRef (mwpPlaythroughHi mwp)
  let BitRack newLo newHi = bitRackAddLetter ml (BitRack lo hi)
  writeSTRef (mwpPlaythroughLo mwp) newLo
  writeSTRef (mwpPlaythroughHi mwp) newHi
  num <- readSTRef (mwpNumTilesPlayedThrough mwp)
  writeSTRef (mwpNumTilesPlayedThrough mwp) (num + 1)

-- | Save playthrough state for later restore
mwmpSave :: MWMPPlaythrough s -> ST s ()
mwmpSave mwp = do
  readSTRef (mwpPlaythroughLo mwp) >>= writeSTRef (mwpPlaythroughLoCopy mwp)
  readSTRef (mwpPlaythroughHi mwp) >>= writeSTRef (mwpPlaythroughHiCopy mwp)
  readSTRef (mwpNumTilesPlayedThrough mwp) >>= writeSTRef (mwpNumTilesPlayedThroughCopy mwp)
  readSTRef (mwpPlaythroughBlocks mwp) >>= writeSTRef (mwpPlaythroughBlocksCopy mwp)

-- | Restore playthrough state from saved copy
mwmpRestore :: MWMPPlaythrough s -> ST s ()
mwmpRestore mwp = do
  readSTRef (mwpPlaythroughLoCopy mwp) >>= writeSTRef (mwpPlaythroughLo mwp)
  readSTRef (mwpPlaythroughHiCopy mwp) >>= writeSTRef (mwpPlaythroughHi mwp)
  readSTRef (mwpNumTilesPlayedThroughCopy mwp) >>= writeSTRef (mwpNumTilesPlayedThrough mwp)
  readSTRef (mwpPlaythroughBlocksCopy mwp) >>= writeSTRef (mwpPlaythroughBlocks mwp)

-- | Increment playthrough blocks count
mwmpIncrementBlocks :: MWMPPlaythrough s -> ST s ()
mwmpIncrementBlocks mwp = do
  blocks <- readSTRef (mwpPlaythroughBlocks mwp)
  writeSTRef (mwpPlaythroughBlocks mwp) (blocks + 1)

-- | Get current playthrough BitRack
mwmpGetPlaythroughBitRack :: MWMPPlaythrough s -> ST s BitRack
mwmpGetPlaythroughBitRack mwp = do
  lo <- readSTRef (mwpPlaythroughLo mwp)
  hi <- readSTRef (mwpPlaythroughHi mwp)
  return (BitRack lo hi)

-- | Get number of tiles played through
mwmpGetNumTilesPlayedThrough :: MWMPPlaythrough s -> ST s Int
mwmpGetNumTilesPlayedThrough mwp = readSTRef (mwpNumTilesPlayedThrough mwp)

-- | Check if full rack bingo exists with current playthrough tiles
mwmpCheckBingoExistence :: WMPStatic -> MWMPPlaythrough s -> ST s Bool
mwmpCheckBingoExistence ws mwp = do
  case wsWMP ws of
    Nothing -> return False
    Just wmp -> do
      playthroughBR <- mwmpGetPlaythroughBitRack mwp
      numPlaythrough <- mwmpGetNumTilesPlayedThrough mwp
      let fullSize = wsFullRackSize ws
          subrack = bitRackAddBitRack (wsPlayerBitRack ws) playthroughBR
          wordSize = fullSize + numPlaythrough
          entry = wmpGetWordEntry wmp subrack wordSize
      return (isJust entry)
