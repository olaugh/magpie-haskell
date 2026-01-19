{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- | BitRack: 128-bit multiset representation for tile racks
-- Ported from MAGPIE's bit_rack.h
module Magpie.BitRack
  ( BitRack(..)
  , emptyBitRack
  , bitRackAddLetter
  , bitRackRemoveLetter
  , bitRackGetLetterCount
  , bitRackSetLetterCount
  , bitRackGetBucketIndex
  , bitRackFromTiles
  , bitRackFromString
  , bitRackEqual
  , bitRackTotalTiles
  , bitRackNumBlanks
  ) where

import Data.Word (Word32, Word64)
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import Magpie.Types (MachineLetter(..))
import Magpie.LetterDistribution (LetterDistribution, ldCharToML)

-- | Constants from bit_rack_defs.h
bitRackBitsPerLetter :: Int
bitRackBitsPerLetter = 4

bitRackMaxCountPerLetter :: Word64
bitRackMaxCountPerLetter = 15  -- 2^4 - 1

-- | BitRack: 128-bit multiset of tiles
-- low holds letters 0-15 (4 bits each)
-- high holds letters 16-31 (4 bits each)
data BitRack = BitRack
  { bitRackLow  :: {-# UNPACK #-} !Word64
  , bitRackHigh :: {-# UNPACK #-} !Word64
  } deriving (Show)

instance Eq BitRack where
  (BitRack l1 h1) == (BitRack l2 h2) = l1 == l2 && h1 == h2

-- | Empty BitRack
{-# INLINE emptyBitRack #-}
emptyBitRack :: BitRack
emptyBitRack = BitRack 0 0

-- | Check if two BitRacks are equal
{-# INLINE bitRackEqual #-}
bitRackEqual :: BitRack -> BitRack -> Bool
bitRackEqual = (==)

-- | Add a machine letter to the BitRack
{-# INLINE bitRackAddLetter #-}
bitRackAddLetter :: MachineLetter -> BitRack -> BitRack
bitRackAddLetter (MachineLetter ml) (BitRack lo hi)
  | ml < 16 =
      let shift = fromIntegral ml * bitRackBitsPerLetter
          inc = 1 `shiftL` shift
      in BitRack (lo + inc) hi
  | otherwise =
      let shift = (fromIntegral ml - 16) * bitRackBitsPerLetter
          inc = 1 `shiftL` shift
      in BitRack lo (hi + inc)

-- | Remove a machine letter from the BitRack (assumes letter is present)
{-# INLINE bitRackRemoveLetter #-}
bitRackRemoveLetter :: MachineLetter -> BitRack -> BitRack
bitRackRemoveLetter (MachineLetter ml) (BitRack lo hi)
  | ml < 16 =
      let shift = fromIntegral ml * bitRackBitsPerLetter
          dec = 1 `shiftL` shift
      in BitRack (lo - dec) hi
  | otherwise =
      let shift = (fromIntegral ml - 16) * bitRackBitsPerLetter
          dec = 1 `shiftL` shift
      in BitRack lo (hi - dec)

-- | Get the count of a specific letter
{-# INLINE bitRackGetLetterCount #-}
bitRackGetLetterCount :: MachineLetter -> BitRack -> Int
bitRackGetLetterCount (MachineLetter ml) (BitRack lo hi)
  | ml < 16 =
      let shift = fromIntegral ml * bitRackBitsPerLetter
      in fromIntegral ((lo `shiftR` shift) .&. bitRackMaxCountPerLetter)
  | otherwise =
      let shift = (fromIntegral ml - 16) * bitRackBitsPerLetter
      in fromIntegral ((hi `shiftR` shift) .&. bitRackMaxCountPerLetter)

-- | Set the count of a specific letter (matching C bit_rack_set_letter_count)
{-# INLINE bitRackSetLetterCount #-}
bitRackSetLetterCount :: MachineLetter -> Int -> BitRack -> BitRack
bitRackSetLetterCount (MachineLetter ml) count (BitRack lo hi)
  | ml < 16 =
      let shift = fromIntegral ml * bitRackBitsPerLetter
          mask = bitRackMaxCountPerLetter `shiftL` shift
          newVal = fromIntegral count `shiftL` shift
      in BitRack ((lo .&. complement mask) .|. newVal) hi
  | otherwise =
      let shift = (fromIntegral ml - 16) * bitRackBitsPerLetter
          mask = bitRackMaxCountPerLetter `shiftL` shift
          newVal = fromIntegral count `shiftL` shift
      in BitRack lo ((hi .&. complement mask) .|. newVal)
  where
    complement x = xor x maxBound

-- | Get the number of blanks in the BitRack (letter 0)
{-# INLINE bitRackNumBlanks #-}
bitRackNumBlanks :: BitRack -> Int
bitRackNumBlanks = bitRackGetLetterCount (MachineLetter 0)

-- | Get total number of tiles in the BitRack
bitRackTotalTiles :: BitRack -> Int
bitRackTotalTiles (BitRack lo hi) = countNibbles lo + countNibbles hi
  where
    countNibbles :: Word64 -> Int
    countNibbles w = sum [ fromIntegral ((w `shiftR` (i * 4)) .&. 0xF)
                         | i <- [0..15] ]

-- | Hash function matching C implementation (bit_rack_mix_to_64 + bit_rack_get_bucket_index)
-- Ported from bit_rack.h
{-# INLINE bitRackGetBucketIndex #-}
bitRackGetBucketIndex :: BitRack -> Int -> Word32
bitRackGetBucketIndex (BitRack lo hi) numBuckets =
  let -- MurmurHash3-style mixing constants
      c1 = 0xff51afd7ed558ccd :: Word64
      c2 = 0xc4ceb9fe1a85ec53 :: Word64
      rotationShift = 17 :: Int

      -- Fold high into low with rotation for better avalanche
      !mixed0 = lo `xor` hi
      !mixed1 = mixed0 `xor` ((hi `shiftL` rotationShift) .|. (hi `shiftR` (64 - rotationShift)))

      -- MurmurHash3-style mixing for bit diffusion
      !mixed2 = mixed1 `xor` (mixed1 `shiftR` 33)
      !mixed3 = mixed2 * c1
      !mixed4 = mixed3 `xor` (mixed3 `shiftR` 33)
      !mixed5 = mixed4 * c2
      !h = mixed5 `xor` (mixed5 `shiftR` 33)

      -- Bucket index: hash & (num_buckets - 1) for power-of-2 buckets
      !bucket = fromIntegral (h .&. fromIntegral (numBuckets - 1))
  in bucket

-- | Create a BitRack from a list of machine letters
bitRackFromTiles :: [MachineLetter] -> BitRack
bitRackFromTiles = foldr bitRackAddLetter emptyBitRack

-- | Create a BitRack from a string using a letter distribution
-- For WMP lookups, we need to handle blanks specially:
-- - '?' represents an undesignated blank (letter 0)
-- - Lowercase letters are NOT designated blanks for WMP purposes,
--   they are treated as regular letters (WMP doesn't track blank designation)
bitRackFromString :: LetterDistribution -> String -> BitRack
bitRackFromString ld = foldr addChar emptyBitRack
  where
    addChar c br =
      let ml = ldCharToML ld c
          -- For WMP, we use the base letter (strip blank bit if any)
          -- because WMP tracks letter multisets, not blank designations
          baseMl = MachineLetter (unML ml .&. 0x7F)
      in bitRackAddLetter baseMl br
