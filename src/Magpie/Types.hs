{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Core types for the Magpie Scrabble engine
module Magpie.Types
  ( -- * Machine Letters
    MachineLetter(..)
  , blankMask
  , isBlank
  , blankLetter
  , unblankLetter
  , playedThroughMarker

    -- * Direction
  , Direction(..)
  , otherDirection

    -- * Position
  , Pos(..)
  , Row
  , Col

    -- * Bonus Squares
  , BonusSquare(..)
  , letterMultiplier
  , wordMultiplier

    -- * Square
  , Square(..)
  , emptySquare

    -- * Rack
  , Rack(..)
  , emptyRack
  , rackSize
  , rackAddLetter
  , rackTakeLetter
  , rackHasLetter
  , rackGetCount
  , rackTotal
  , rackFromList
  , rackToList

    -- * Move
  , Move(..)
  , MoveType(..)

    -- * Board dimensions
  , boardDim
  , defaultRackSize
  ) where

import Data.Word (Word8, Word64)
import Data.Int (Int32)
import Data.Bits ((.&.), (.|.), testBit)
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)

-- | Board dimension (standard Scrabble = 15)
boardDim :: Int
boardDim = 15

-- | Default rack size
defaultRackSize :: Int
defaultRackSize = 7

-- | A machine letter is a compact representation of a tile
-- 0 = blank/empty, 1-26 = A-Z for English
-- High bit (0x80) indicates a blanked letter
newtype MachineLetter = MachineLetter { unML :: Word8 }
  deriving (Eq, Ord, Show, Generic)

-- | Mask for blank designation
blankMask :: Word8
blankMask = 0x80

-- | Played-through marker (same as empty square)
playedThroughMarker :: MachineLetter
playedThroughMarker = MachineLetter 0

-- | Check if a letter is blanked
isBlank :: MachineLetter -> Bool
isBlank (MachineLetter ml) = testBit ml 7

-- | Mark a letter as played with a blank
blankLetter :: MachineLetter -> MachineLetter
blankLetter (MachineLetter ml) = MachineLetter (ml .|. blankMask)

-- | Get the underlying letter from a blanked letter
unblankLetter :: MachineLetter -> MachineLetter
unblankLetter (MachineLetter ml) = MachineLetter (ml .&. 0x7F)

-- | Direction of play
data Direction = Horizontal | Vertical
  deriving (Eq, Show, Ord, Enum, Generic)

-- | Get the other direction
otherDirection :: Direction -> Direction
otherDirection Horizontal = Vertical
otherDirection Vertical = Horizontal

-- | Row index (0-based)
type Row = Int

-- | Column index (0-based)
type Col = Int

-- | Board position
data Pos = Pos !Row !Col
  deriving (Eq, Show, Ord, Generic)

-- | Bonus square types
data BonusSquare
  = NoBonus
  | DoubleLetter
  | TripleLetter
  | DoubleWord
  | TripleWord
  deriving (Eq, Show, Ord, Enum, Generic)

-- | Get letter multiplier for a bonus square
letterMultiplier :: BonusSquare -> Int
letterMultiplier DoubleLetter = 2
letterMultiplier TripleLetter = 3
letterMultiplier _ = 1

-- | Get word multiplier for a bonus square
wordMultiplier :: BonusSquare -> Int
wordMultiplier DoubleWord = 2
wordMultiplier TripleWord = 3
wordMultiplier _ = 1

-- | A square on the board
data Square = Square
  { sqLetter           :: !MachineLetter  -- ^ Letter on this square (0 = empty)
  , sqBonus            :: !BonusSquare    -- ^ Bonus type
  , sqCrossSet         :: !Word64         -- ^ Valid cross letters (bit set)
  , sqCrossScore       :: !Int            -- ^ Score from perpendicular tiles
  , sqIsAnchor         :: !Bool           -- ^ Is this an anchor square?
  , sqLeftExtensionSet :: !Word64         -- ^ Letters that can extend leftward
  , sqRightExtensionSet :: !Word64        -- ^ Letters that can extend rightward
  } deriving (Eq, Show, Generic)

-- | An empty square with no bonus
emptySquare :: Square
emptySquare = Square
  { sqLetter = MachineLetter 0
  , sqBonus = NoBonus
  , sqCrossSet = 0
  , sqCrossScore = 0
  , sqIsAnchor = False
  , sqLeftExtensionSet = 0
  , sqRightExtensionSet = 0
  }

-- | A rack of tiles (multiset of machine letters)
-- Uses a vector where index = machine letter, value = count
data Rack = Rack
  { rackCounts :: !(VU.Vector Int)  -- ^ Count of each letter
  , rackTotal_ :: !Int               -- ^ Total tiles on rack
  , rackDistSize :: !Int             -- ^ Size of the letter distribution
  } deriving (Eq, Show, Generic)

-- | Create an empty rack
emptyRack :: Int -> Rack
emptyRack distSize = Rack
  { rackCounts = VU.replicate distSize 0
  , rackTotal_ = 0
  , rackDistSize = distSize
  }

-- | Maximum rack size (for allocation)
rackSize :: Int
rackSize = 7

-- | Add a letter to the rack
rackAddLetter :: MachineLetter -> Rack -> Rack
rackAddLetter (MachineLetter ml) rack =
  let idx = fromIntegral ml
      counts = rackCounts rack
      newCount = (counts VU.! idx) + 1
  in rack { rackCounts = counts VU.// [(idx, newCount)]
          , rackTotal_ = rackTotal_ rack + 1
          }

-- | Take a letter from the rack
rackTakeLetter :: MachineLetter -> Rack -> Rack
rackTakeLetter (MachineLetter ml) rack =
  let idx = fromIntegral ml
      counts = rackCounts rack
      newCount = (counts VU.! idx) - 1
  in rack { rackCounts = counts VU.// [(idx, newCount)]
          , rackTotal_ = rackTotal_ rack - 1
          }

-- | Check if rack has at least one of a letter
rackHasLetter :: MachineLetter -> Rack -> Bool
rackHasLetter (MachineLetter ml) rack =
  (rackCounts rack VU.! fromIntegral ml) > 0

-- | Get count of a specific letter
rackGetCount :: MachineLetter -> Rack -> Int
rackGetCount (MachineLetter ml) rack =
  rackCounts rack VU.! fromIntegral ml

-- | Get total number of tiles on rack
rackTotal :: Rack -> Int
rackTotal = rackTotal_

-- | Create a rack from a list of machine letters
rackFromList :: Int -> [MachineLetter] -> Rack
rackFromList distSize = foldr rackAddLetter (emptyRack distSize)

-- | Convert rack to list of machine letters
rackToList :: Rack -> [MachineLetter]
rackToList rack =
  [ MachineLetter (fromIntegral i)
  | i <- [0 .. rackDistSize rack - 1]
  , _ <- [1 .. rackCounts rack VU.! i]
  ]

-- | Move type
data MoveType
  = TilePlacement
  | Exchange
  | Pass
  deriving (Eq, Show, Ord, Enum, Generic)

-- | A move in the game
data Move = Move
  { moveType       :: !MoveType
  , moveRow        :: !Row
  , moveCol        :: !Col
  , moveDir        :: !Direction
  , moveTiles      :: ![MachineLetter]  -- ^ Tiles played (0 = play-through)
  , moveTilesUsed  :: !Int              -- ^ Number of tiles from rack
  , moveScore      :: !Int
  , moveEquity     :: !Int32            -- ^ Score + leave value (fixed-point, 1000x resolution)
  } deriving (Eq, Show, Generic)
