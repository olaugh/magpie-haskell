-- | Anagram finding using a KWG DAWG

module Anagram
  ( findAnagrams
  , findSubAnagrams
  ) where

import KWG
import Data.Char (ord)
import Data.Word (Word32)
import Data.List (sort)
import qualified Data.Map.Strict as M

-- | Letter counts for the rack
type Rack = M.Map Word32 Int

-- | Convert a character to a tile number (1=A, 2=B, ..., 26=Z, 0=blank)
charToTile :: Char -> Maybe Word32
charToTile c
  | c >= 'A' && c <= 'Z' = Just $ fromIntegral (ord c - ord 'A' + 1)
  | c >= 'a' && c <= 'z' = Just $ fromIntegral (ord c - ord 'a' + 1)
  | c == '?' || c == '_' = Just 0  -- blank tile
  | otherwise = Nothing

-- | Convert a tile number to a character
tileToChar :: Word32 -> Char
tileToChar 0 = '?'
tileToChar n = toEnum (fromIntegral n + ord 'A' - 1)

-- | Convert a string to a rack (letter counts)
stringToRack :: String -> Rack
stringToRack = foldr addTile M.empty
  where
    addTile c rack = case charToTile c of
      Just tile -> M.insertWith (+) tile 1 rack
      Nothing -> rack

-- | Find all anagrams (exact matches - use all letters)
findAnagrams :: KWG -> String -> [String]
findAnagrams kwg letters =
  let rack = stringToRack letters
      rackSize = sum (M.elems rack)
  in filter (\w -> length w == rackSize) $ findSubAnagrams kwg letters

-- | Find all sub-anagrams (words using some or all letters)
findSubAnagrams :: KWG -> String -> [String]
findSubAnagrams kwg letters =
  let rack = stringToRack letters
      root = dawgRoot kwg
  in sort $ search kwg rack root []

-- | Recursive search through the DAWG
search :: KWG -> Rack -> Word32 -> String -> [String]
search kwg rack nodeIdx prefix
  | nodeIdx == 0 = []  -- null pointer
  | otherwise = searchSiblings kwg rack nodeIdx prefix

-- | Search through siblings at the current node position
searchSiblings :: KWG -> Rack -> Word32 -> String -> [String]
searchSiblings kwg rack nodeIdx prefix =
  let node = getNode kwg nodeIdx
      tile = nodeTile node
      arcIdx = nodeArcIndex node
      isEnd = nodeIsEnd node
      accepts = nodeAccepts node

      -- Try to use this tile from our rack
      results = tryTile kwg rack tile arcIdx accepts prefix

      -- Also try blank if we have one and this isn't the blank tile itself
      blankResults = if tile /= 0 then tryBlank kwg rack tile arcIdx accepts prefix else []

      -- Continue to next sibling if not at end
      siblingResults = if isEnd then [] else searchSiblings kwg rack (nodeIdx + 1) prefix

  in results ++ blankResults ++ siblingResults

-- | Try to use a specific tile from the rack
tryTile :: KWG -> Rack -> Word32 -> Word32 -> Bool -> String -> [String]
tryTile kwg rack tile arcIdx accepts prefix =
  case M.lookup tile rack of
    Nothing -> []
    Just count | count <= 0 -> []
    Just count ->
      let newRack = if count == 1
                    then M.delete tile rack
                    else M.insert tile (count - 1) rack
          newPrefix = prefix ++ [tileToChar tile]
          -- If this node accepts, we found a word
          foundWords = if accepts then [newPrefix] else []
          -- Continue searching from the arc index
          childWords = if arcIdx /= 0 then search kwg newRack arcIdx newPrefix else []
      in foundWords ++ childWords

-- | Try to use a blank tile as the given letter
tryBlank :: KWG -> Rack -> Word32 -> Word32 -> Bool -> String -> [String]
tryBlank kwg rack tile arcIdx accepts prefix =
  case M.lookup 0 rack of  -- 0 is the blank tile
    Nothing -> []
    Just count | count <= 0 -> []
    Just count ->
      let newRack = if count == 1
                    then M.delete 0 rack
                    else M.insert 0 (count - 1) rack
          -- Show blank as lowercase
          newPrefix = prefix ++ [tileToChar tile]
          -- If this node accepts, we found a word
          foundWords = if accepts then [newPrefix] else []
          -- Continue searching from the arc index
          childWords = if arcIdx /= 0 then search kwg newRack arcIdx newPrefix else []
      in foundWords ++ childWords
