{-# LANGUAGE BangPatterns #-}

-- | KWG (Kurnia Word Graph) format reader and traversal
-- Based on the format from https://github.com/andy-k/wolges
-- and the MAGPIE implementation
--
-- A KWG contains both a DAWG (for word lookup) and a GADDAG (for move generation).
-- Each node is a 32-bit value encoding:
--   - Bits 0-21:  Arc index (pointer to first child)
--   - Bit 22:     Is this the last sibling in the node set?
--   - Bit 23:     Does reaching this node complete a valid word?
--   - Bits 24-31: The tile/letter value (0 = separator, 1 = A, etc.)

module Magpie.KWG
  ( KWG
  , loadKWG
  , dawgRoot
  , gaddagRoot
  , nodeArcIndex
  , nodeTile
  , nodeIsEnd
  , nodeAccepts
  , getNode
  , numNodes

    -- * Traversal
  , getNextNodeIndex
  , getLetterSet
  , isWordValid
  , checkAccepts
  , findWords
  , WordCallback
  ) where

import Magpie.Types (MachineLetter(..))

import qualified Data.ByteString as BS
import Data.Word (Word32, Word64, Word8)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import qualified Data.Vector.Unboxed as V

-- | A KWG is a vector of 32-bit nodes
newtype KWG = KWG (V.Vector Word32)
  deriving (Show)

-- | Callback for word enumeration: receives the word and returns whether to continue
type WordCallback = [MachineLetter] -> IO Bool

-- | Load a KWG file (little-endian 32-bit words)
loadKWG :: FilePath -> IO KWG
loadKWG path = do
  bs <- BS.readFile path
  let nodes = bytesToWord32s (BS.unpack bs)
  return $ KWG (V.fromList nodes)

-- | Convert bytes to little-endian Word32s
bytesToWord32s :: [Word8] -> [Word32]
bytesToWord32s [] = []
bytesToWord32s (a:b:c:d:rest) =
  let w = fromIntegral a
        + fromIntegral b * 256
        + fromIntegral c * 65536
        + fromIntegral d * 16777216
  in w : bytesToWord32s rest
bytesToWord32s _ = []

-- | Get a node by index
{-# INLINE getNode #-}
getNode :: KWG -> Word32 -> Word32
getNode (KWG nodes) idx
  | fromIntegral idx < V.length nodes = nodes `V.unsafeIndex` fromIntegral idx
  | otherwise = 0

-- | Number of nodes
numNodes :: KWG -> Int
numNodes (KWG nodes) = V.length nodes

-- | Get the DAWG root node index (from node 0)
{-# INLINE dawgRoot #-}
dawgRoot :: KWG -> Word32
dawgRoot kwg = nodeArcIndex (getNode kwg 0)

-- | Get the GADDAG root node index (from node 1)
{-# INLINE gaddagRoot #-}
gaddagRoot :: KWG -> Word32
gaddagRoot kwg = nodeArcIndex (getNode kwg 1)

-- | Extract the arc index (pointer to children) from a node
{-# INLINE nodeArcIndex #-}
nodeArcIndex :: Word32 -> Word32
nodeArcIndex node = node .&. 0x3FFFFF

-- | Extract the tile/letter from a node
{-# INLINE nodeTile #-}
nodeTile :: Word32 -> Word32
nodeTile node = node `shiftR` 24

-- | Check if this is the last sibling in a node set
{-# INLINE nodeIsEnd #-}
nodeIsEnd :: Word32 -> Bool
nodeIsEnd node = testBit node 22

-- | Check if this node accepts (completes a valid word)
{-# INLINE nodeAccepts #-}
nodeAccepts :: Word32 -> Bool
nodeAccepts node = testBit node 23

-- | Get the next node index for a given letter
-- Returns 0 if the letter is not found
{-# INLINE getNextNodeIndex #-}
getNextNodeIndex :: KWG -> Word32 -> MachineLetter -> Word32
getNextNodeIndex kwg nodeIdx (MachineLetter letter)
  | nodeIdx == 0 = 0
  | otherwise = go nodeIdx
  where
    letterW = fromIntegral letter
    go !i =
      let node = getNode kwg i
          tile = nodeTile node
      in if tile == letterW
         then nodeArcIndex node
         else if nodeIsEnd node
              then 0
              else go (i + 1)

-- | Get the set of valid letters at a node as a bitmask (Word64)
-- Also returns whether each letter would complete a word
getLetterSet :: KWG -> Word32 -> (Word64, Word64)
getLetterSet kwg nodeIdx
  | nodeIdx == 0 = (0, 0)
  | otherwise = go nodeIdx 0 0
  where
    go !i !extSet !acceptSet =
      let node = getNode kwg i
          tile = nodeTile node
          bit :: Word64
          bit = if tile == 0 then 0 else 1 `shiftL` fromIntegral tile
          extSet' = extSet .|. bit
          acceptSet' = if nodeAccepts node then acceptSet .|. bit else acceptSet
      in if nodeIsEnd node
         then (extSet', acceptSet')
         else go (i + 1) extSet' acceptSet'

-- | Check if a word is valid in the DAWG
isWordValid :: KWG -> [MachineLetter] -> Bool
isWordValid kwg word = go (dawgRoot kwg) word
  where
    go _ [] = False  -- Empty word is not valid
    go nodeIdx [ml] =
      -- Last letter: check if it accepts
      checkAccepts kwg nodeIdx ml
    go nodeIdx (ml:rest) =
      let nextIdx = getNextNodeIndex kwg nodeIdx ml
      in if nextIdx == 0
         then False
         else go nextIdx rest

-- | Check if a letter at this node accepts
checkAccepts :: KWG -> Word32 -> MachineLetter -> Bool
checkAccepts kwg nodeIdx (MachineLetter letter)
  | nodeIdx == 0 = False
  | otherwise = go nodeIdx
  where
    letterW = fromIntegral letter
    go !i =
      let node = getNode kwg i
          tile = nodeTile node
      in if tile == letterW
         then nodeAccepts node
         else if nodeIsEnd node
              then False
              else go (i + 1)

-- | Find all words that can be formed starting from a node
-- Used for debugging and testing
findWords :: KWG -> Word32 -> [[MachineLetter]]
findWords kwg startIdx
  | startIdx == 0 = []
  | otherwise = go startIdx []
  where
    go nodeIdx prefix = collectSiblings nodeIdx prefix

    collectSiblings i prefix =
      let node = getNode kwg i
          tile = nodeTile node
          ml = MachineLetter (fromIntegral tile)
          newPrefix = prefix ++ [ml]
          arcIdx = nodeArcIndex node
          -- Words formed by continuing from this node
          childWords = if arcIdx /= 0 then go arcIdx newPrefix else []
          -- This node itself may complete a word
          thisWord = if nodeAccepts node then [newPrefix] else []
          -- Continue to next sibling
          siblingWords = if nodeIsEnd node then [] else collectSiblings (i + 1) prefix
      in thisWord ++ childWords ++ siblingWords
