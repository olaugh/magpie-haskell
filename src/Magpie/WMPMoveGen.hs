{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | WMP-based move generation
-- Ported from MAGPIE's wmp_move_gen.h
--
-- This module provides efficient move generation using the Word Map (WMP)
-- data structure. It enumerates all subracks of the player's rack, checks
-- which form valid words via WMP lookup, and tracks playthrough tiles.
module Magpie.WMPMoveGen
  ( -- * Types
    WMPMoveGen(..)
  , SubrackInfo(..)

    -- * Initialization
  , wmpMoveGenInit
  , wmpMoveGenIsActive

    -- * Playthrough management
  , wmpMoveGenResetPlaythrough
  , wmpMoveGenHasPlaythrough
  , wmpMoveGenAddPlaythroughLetter
  , wmpMoveGenSavePlaythroughState
  , wmpMoveGenRestorePlaythroughState
  , wmpMoveGenIncrementPlaythroughBlocks

    -- * Word existence checking
  , wmpMoveGenCheckNonplaythroughExistence
  , wmpMoveGenNonplaythroughWordOfLengthExists
  , wmpMoveGenGetNonplaythroughBestLeaveValues
  , wmpMoveGenCheckPlaythroughFullRackExistence

    -- * Subrack iteration
  , wmpMoveGenPlaythroughSubracksInit
  , wmpMoveGenGetNumSubrackCombinations
  , wmpMoveGenGetSubrackWords
  , wmpMoveGenGetWord
  , wmpMoveGenGetLeaveValue

    -- * Constants
  , minimumWordLength
  , rackSize
  ) where

import Magpie.BitRack
import Magpie.WMP
import Magpie.Equity (Equity(..))
import Magpie.Types (MachineLetter(..), Rack(..))

import Data.Bits (shiftL)
import Data.Maybe (isJust)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)

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

-- | Convert Rack to BitRack
rackToBitRack :: Rack -> BitRack
rackToBitRack rack =
  let counts = rackCounts rack
      addLetters br ml =
        let count = counts VU.! ml
        in iterate (bitRackAddLetter (MachineLetter (fromIntegral ml))) br !! count
  in foldl addLetters emptyBitRack [0 .. VU.length counts - 1]

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
