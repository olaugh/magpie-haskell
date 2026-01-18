{-# LANGUAGE OverloadedStrings #-}

-- | Letter distribution for tile bag and scoring
module Magpie.LetterDistribution
  ( LetterDistribution(..)
  , loadLetterDistribution
  , defaultEnglishLD
  , ldSize
  , ldScore
  , ldCount
  , ldIsVowel
  , ldToChar
  , ldFromChar
  , ldTotalTiles
  , ldToString
  , ldFromString
  , ldCharToML
  ) where

import Magpie.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Bits ((.&.))
import Data.Char (toUpper)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)

-- | Letter distribution configuration
data LetterDistribution = LetterDistribution
  { ldName_       :: !String
  , ldSize_       :: !Int                  -- ^ Number of distinct letters (including blank)
  , ldScores      :: !(VU.Vector Int)      -- ^ Score for each machine letter
  , ldCounts      :: !(VU.Vector Int)      -- ^ Count in bag for each letter
  , ldIsVowels    :: !(VU.Vector Bool)     -- ^ Is this letter a vowel?
  , ldChars       :: !(V.Vector String)    -- ^ Human-readable form (uppercase)
  , ldCharsLower  :: !(V.Vector String)    -- ^ Human-readable form (lowercase)
  , ldTotalTiles_ :: !Int                  -- ^ Total tiles in the bag
  } deriving (Show)

-- | Get the distribution size (number of letter types)
{-# INLINE ldSize #-}
ldSize :: LetterDistribution -> Int
ldSize = ldSize_

-- | Get the score for a machine letter
{-# INLINE ldScore #-}
ldScore :: LetterDistribution -> MachineLetter -> Int
ldScore ld (MachineLetter ml) = ldScores ld `VU.unsafeIndex` fromIntegral (ml .&. 0x7F)

-- | Get the count in bag for a machine letter
{-# INLINE ldCount #-}
ldCount :: LetterDistribution -> MachineLetter -> Int
ldCount ld (MachineLetter ml) = ldCounts ld `VU.unsafeIndex` fromIntegral ml

-- | Check if a machine letter is a vowel
{-# INLINE ldIsVowel #-}
ldIsVowel :: LetterDistribution -> MachineLetter -> Bool
ldIsVowel ld (MachineLetter ml) = ldIsVowels ld `VU.unsafeIndex` fromIntegral (ml .&. 0x7F)

-- | Get total tiles in the distribution
ldTotalTiles :: LetterDistribution -> Int
ldTotalTiles = ldTotalTiles_

-- | Convert machine letter to character
ldToChar :: LetterDistribution -> MachineLetter -> Char
ldToChar ld ml@(MachineLetter mlVal)
  | mlVal == 0 = '?'
  | isBlank ml = head $ ldCharsLower ld V.! fromIntegral (mlVal .&. 0x7F)
  | otherwise  = head $ ldChars ld V.! fromIntegral mlVal

-- | Convert character to machine letter
ldFromChar :: LetterDistribution -> Char -> Maybe MachineLetter
ldFromChar ld c
  | c == '?' = Just $ MachineLetter 0
  | otherwise =
      let upper = toUpper c
          idx = V.findIndex (\s -> not (null s) && head s == upper) (ldChars ld)
      in case idx of
           Just i -> Just $ MachineLetter $ fromIntegral i
           Nothing -> Nothing

-- | Convert machine letters to string
ldToString :: LetterDistribution -> [MachineLetter] -> String
ldToString ld = map (ldToChar ld)

-- | Convert string to machine letters
ldFromString :: LetterDistribution -> String -> Maybe [MachineLetter]
ldFromString ld = traverse (ldFromChar ld)

-- | Convert character to machine letter (with blank support)
-- Uppercase letters -> natural tiles
-- Lowercase letters -> blanks designated as that letter
-- '?' -> blank (value 0)
-- Unknown characters -> blank
ldCharToML :: LetterDistribution -> Char -> MachineLetter
ldCharToML ld c
  | c == '?' = MachineLetter 0  -- Blank
  | c >= 'a' && c <= 'z' =      -- Lowercase = blank designated as letter
      case ldFromChar ld (toUpper c) of
        Just (MachineLetter ml) -> MachineLetter (ml + 0x80)  -- Set blank bit
        Nothing -> MachineLetter 0
  | otherwise =                  -- Uppercase = natural tile
      case ldFromChar ld c of
        Just ml -> ml
        Nothing -> MachineLetter 0  -- Unknown -> blank

-- | Load letter distribution from CSV file
loadLetterDistribution :: FilePath -> IO LetterDistribution
loadLetterDistribution path = do
  content <- BS.readFile path
  let ls = filter (not . BS.null) $ C8.lines content
      entries = map parseLine ls
      n = length entries
      scores = VU.fromList $ map (\(_, _, _, s, _) -> s) entries
      counts = VU.fromList $ map (\(_, _, c, _, _) -> c) entries
      vowels = VU.fromList $ map (\(_, _, _, _, v) -> v) entries
      chars = V.fromList $ map (\(u, _, _, _, _) -> u) entries
      charsLower = V.fromList $ map (\(_, l, _, _, _) -> l) entries
      total = sum $ map (\(_, _, c, _, _) -> c) entries
  return $ LetterDistribution
    { ldName_ = path
    , ldSize_ = n
    , ldScores = scores
    , ldCounts = counts
    , ldIsVowels = vowels
    , ldChars = chars
    , ldCharsLower = charsLower
    , ldTotalTiles_ = total
    }

-- | Parse a single line of the CSV
parseLine :: BS.ByteString -> (String, String, Int, Int, Bool)
parseLine line =
  let parts = C8.split ',' line
  in case parts of
       (upper : lower : count : score : vowel : _) ->
         ( C8.unpack upper
         , C8.unpack lower
         , read (C8.unpack count)
         , read (C8.unpack score)
         , C8.unpack vowel == "1"
         )
       _ -> error $ "Invalid letter distribution line: " ++ C8.unpack line

-- | Default English letter distribution (hardcoded for convenience)
defaultEnglishLD :: LetterDistribution
defaultEnglishLD = LetterDistribution
  { ldName_ = "english"
  , ldSize_ = 27
  , ldScores = VU.fromList
      [0, 1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10]
      -- ?  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q   R  S  T  U  V  W  X  Y  Z
  , ldCounts = VU.fromList
      [2, 9, 2, 2, 4, 12, 2, 3, 2, 9, 1, 1, 4, 2, 6, 8, 2, 1, 6, 4, 6, 4, 2, 2, 1, 2, 1]
  , ldIsVowels = VU.fromList
      [False, True, False, False, False, True, False, False, False, True,
       False, False, False, False, False, True, False, False, False, False,
       False, True, False, False, False, False, False]
  , ldChars = V.fromList
      ["?", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
       "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"]
  , ldCharsLower = V.fromList
      ["?", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
       "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
  , ldTotalTiles_ = 100
  }
