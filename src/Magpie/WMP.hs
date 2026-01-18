{-# LANGUAGE BangPatterns #-}

-- | Word Map (WMP): Fast anagram lookup structure
-- Ported from MAGPIE's wmp.h
module Magpie.WMP
  ( -- * Types
    WMP(..)
  , WMPForLength(..)
  , WMPEntry(..)

    -- * Loading
  , loadWMP

    -- * Querying
  , wmpGetWordEntry
  , wmpWriteWordsToBuffer

    -- * Entry inspection
  , wmpEntryIsInlined
  , wmpEntryReadBitRack
  , wmpEntryBlankLetters

    -- * Constants
  , wmpInlineValueBytes
  , wmpBitRackBytes
  , wmpEarliestSupportedVersion
  ) where

import Magpie.BitRack
import Magpie.Types (MachineLetter(..), boardDim)

import Control.Monad (when)
import Data.Bits (shiftL, testBit)
import Data.Word (Word8, Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL

-- | Constants from wmp_defs.h
wmpInlineValueBytes :: Int
wmpInlineValueBytes = 16

wmpBitRackBytes :: Int
wmpBitRackBytes = 16

wmpEarliestSupportedVersion :: Word8
wmpEarliestSupportedVersion = 3

-- | WMP entry: 32 bytes
-- First 16 bytes: either inline word data or pointers to uninlined data
-- Last 16 bytes: BitRack for collision detection
data WMPEntry = WMPEntry
  { wmpEntryInlineOrPointer :: {-# UNPACK #-} !BS.ByteString  -- 16 bytes
  , wmpEntryBitRackLow      :: {-# UNPACK #-} !Word64
  , wmpEntryBitRackHigh     :: {-# UNPACK #-} !Word64
  } deriving (Show)

-- | Check if entry has inline data (first byte is nonzero)
{-# INLINE wmpEntryIsInlined #-}
wmpEntryIsInlined :: WMPEntry -> Bool
wmpEntryIsInlined entry = BS.index (wmpEntryInlineOrPointer entry) 0 /= 0

-- | Read BitRack from entry
{-# INLINE wmpEntryReadBitRack #-}
wmpEntryReadBitRack :: WMPEntry -> BitRack
wmpEntryReadBitRack entry = BitRack (wmpEntryBitRackLow entry) (wmpEntryBitRackHigh entry)

-- | Get word_start from non-inlined entry (bytes 8-11)
{-# INLINE wmpEntryWordStart #-}
wmpEntryWordStart :: WMPEntry -> Word32
wmpEntryWordStart entry =
  let bs = wmpEntryInlineOrPointer entry
  in fromIntegral (BS.index bs 8)
     + fromIntegral (BS.index bs 9) `shiftL` 8
     + fromIntegral (BS.index bs 10) `shiftL` 16
     + fromIntegral (BS.index bs 11) `shiftL` 24

-- | Get num_words from non-inlined entry (bytes 12-15)
{-# INLINE wmpEntryNumWords #-}
wmpEntryNumWords :: WMPEntry -> Word32
wmpEntryNumWords entry =
  let bs = wmpEntryInlineOrPointer entry
  in fromIntegral (BS.index bs 12)
     + fromIntegral (BS.index bs 13) `shiftL` 8
     + fromIntegral (BS.index bs 14) `shiftL` 16
     + fromIntegral (BS.index bs 15) `shiftL` 24

-- | Get blank_letters bitvector from blank entry (bytes 8-11)
{-# INLINE wmpEntryBlankLetters #-}
wmpEntryBlankLetters :: WMPEntry -> Word32
wmpEntryBlankLetters = wmpEntryWordStart  -- Same offset

-- | WMP data for words of a specific length
data WMPForLength = WMPForLength
  { -- Blankless words
    wflNumWordBuckets    :: !Word32
  , wflWordBucketStarts  :: !(VU.Vector Word32)
  , wflNumWordEntries    :: !Word32
  , wflWordMapEntries    :: !(V.Vector WMPEntry)
  , wflNumUninlinedWords :: !Word32
  , wflWordLetters       :: !BS.ByteString  -- Raw machine letters

    -- Single blanks
  , wflNumBlankBuckets   :: !Word32
  , wflBlankBucketStarts :: !(VU.Vector Word32)
  , wflNumBlankEntries   :: !Word32
  , wflBlankMapEntries   :: !(V.Vector WMPEntry)

    -- Double blanks
  , wflNumDoubleBuckets  :: !Word32
  , wflDoubleBucketStarts :: !(VU.Vector Word32)
  , wflNumDoubleEntries  :: !Word32
  , wflDoubleMapEntries  :: !(V.Vector WMPEntry)
  } deriving (Show)

-- | Empty WMPForLength for unused slots
emptyWMPForLength :: WMPForLength
emptyWMPForLength = WMPForLength
  { wflNumWordBuckets = 0
  , wflWordBucketStarts = VU.empty
  , wflNumWordEntries = 0
  , wflWordMapEntries = V.empty
  , wflNumUninlinedWords = 0
  , wflWordLetters = BS.empty
  , wflNumBlankBuckets = 0
  , wflBlankBucketStarts = VU.empty
  , wflNumBlankEntries = 0
  , wflBlankMapEntries = V.empty
  , wflNumDoubleBuckets = 0
  , wflDoubleBucketStarts = VU.empty
  , wflNumDoubleEntries = 0
  , wflDoubleMapEntries = V.empty
  }

-- | Top-level WMP structure
data WMP = WMP
  { wmpName              :: !String
  , wmpVersion           :: !Word8
  , wmpBoardDim          :: !Word8
  , wmpMaxWordLookupBytes :: !Word32
  , wmpForLengths        :: !(V.Vector WMPForLength)  -- Indexed by word length
  } deriving (Show)

-- | Load WMP from a binary file
loadWMP :: FilePath -> IO WMP
loadWMP path = do
  contents <- BL.readFile path
  let result = runGetOrFail parseWMP contents
  case result of
    Left (_, _, err) -> error $ "Failed to load WMP from " ++ path ++ ": " ++ err
    Right (_, _, wmp) -> return wmp { wmpName = path }

-- | Parse WMP from binary data
parseWMP :: Get WMP
parseWMP = do
  version <- getWord8
  when (version < wmpEarliestSupportedVersion) $
    fail $ "WMP version " ++ show version ++ " not supported, need >= " ++ show wmpEarliestSupportedVersion
  boardDimVal <- getWord8
  when (boardDimVal /= fromIntegral boardDim) $
    fail $ "WMP board dim " ++ show boardDimVal ++ " doesn't match expected " ++ show boardDim
  maxLookupBytes <- getWord32le

  -- Parse WMPForLength for lengths 2 through boardDim
  -- Slots 0 and 1 are unused (not stored in file)
  wflsParsed <- mapM parseWMPForLength [2 .. boardDim]
  let wfls = V.fromList $ emptyWMPForLength : emptyWMPForLength : wflsParsed

  return WMP
    { wmpName = ""
    , wmpVersion = version
    , wmpBoardDim = boardDimVal
    , wmpMaxWordLookupBytes = maxLookupBytes
    , wmpForLengths = wfls
    }

-- | Parse WMPForLength for a specific word length
parseWMPForLength :: Int -> Get WMPForLength
parseWMPForLength wordLen = do
  -- Blankless words
  numWordBuckets <- getWord32le
  wordBucketStarts <- VU.replicateM (fromIntegral numWordBuckets + 1) getWord32le
  numWordEntries <- getWord32le
  wordMapEntries <- V.replicateM (fromIntegral numWordEntries) parseWMPEntry
  numUninlinedWords <- getWord32le
  wordLetters <- getByteString (fromIntegral numUninlinedWords * wordLen)

  -- Single blanks
  numBlankBuckets <- getWord32le
  blankBucketStarts <- VU.replicateM (fromIntegral numBlankBuckets + 1) getWord32le
  numBlankEntries <- getWord32le
  blankMapEntries <- V.replicateM (fromIntegral numBlankEntries) parseWMPEntry

  -- Double blanks
  numDoubleBuckets <- getWord32le
  doubleBucketStarts <- VU.replicateM (fromIntegral numDoubleBuckets + 1) getWord32le
  numDoubleEntries <- getWord32le
  doubleMapEntries <- V.replicateM (fromIntegral numDoubleEntries) parseWMPEntry

  return WMPForLength
    { wflNumWordBuckets = numWordBuckets
    , wflWordBucketStarts = wordBucketStarts
    , wflNumWordEntries = numWordEntries
    , wflWordMapEntries = wordMapEntries
    , wflNumUninlinedWords = numUninlinedWords
    , wflWordLetters = wordLetters
    , wflNumBlankBuckets = numBlankBuckets
    , wflBlankBucketStarts = blankBucketStarts
    , wflNumBlankEntries = numBlankEntries
    , wflBlankMapEntries = blankMapEntries
    , wflNumDoubleBuckets = numDoubleBuckets
    , wflDoubleBucketStarts = doubleBucketStarts
    , wflNumDoubleEntries = numDoubleEntries
    , wflDoubleMapEntries = doubleMapEntries
    }

-- | Parse a single WMPEntry (32 bytes)
parseWMPEntry :: Get WMPEntry
parseWMPEntry = do
  inlineOrPtr <- getByteString 16
  brLow <- getWord64le
  brHigh <- getWord64le
  return WMPEntry
    { wmpEntryInlineOrPointer = inlineOrPtr
    , wmpEntryBitRackLow = brLow
    , wmpEntryBitRackHigh = brHigh
    }

-- | Get word entry for a blankless BitRack
wflGetWordEntry :: WMPForLength -> BitRack -> Maybe WMPEntry
wflGetWordEntry wfl bitRack
  | wflNumWordBuckets wfl == 0 = Nothing
  | otherwise =
      let bucketIdx = bitRackGetBucketIndex bitRack (fromIntegral $ wflNumWordBuckets wfl)
          start = wflWordBucketStarts wfl VU.! fromIntegral bucketIdx
          end = wflWordBucketStarts wfl VU.! (fromIntegral bucketIdx + 1)
      in searchBucket (wflWordMapEntries wfl) bitRack (fromIntegral start) (fromIntegral end)

-- | Get entry for single-blank BitRack
wflGetBlankEntry :: WMPForLength -> BitRack -> Maybe WMPEntry
wflGetBlankEntry wfl bitRack
  | wflNumBlankBuckets wfl == 0 = Nothing
  | otherwise =
      let bucketIdx = bitRackGetBucketIndex bitRack (fromIntegral $ wflNumBlankBuckets wfl)
          start = wflBlankBucketStarts wfl VU.! fromIntegral bucketIdx
          end = wflBlankBucketStarts wfl VU.! (fromIntegral bucketIdx + 1)
      in searchBucket (wflBlankMapEntries wfl) bitRack (fromIntegral start) (fromIntegral end)

-- | Get entry for double-blank BitRack
wflGetDoubleBlankEntry :: WMPForLength -> BitRack -> Maybe WMPEntry
wflGetDoubleBlankEntry wfl bitRack
  | wflNumDoubleBuckets wfl == 0 = Nothing
  | otherwise =
      let bucketIdx = bitRackGetBucketIndex bitRack (fromIntegral $ wflNumDoubleBuckets wfl)
          start = wflDoubleBucketStarts wfl VU.! fromIntegral bucketIdx
          end = wflDoubleBucketStarts wfl VU.! (fromIntegral bucketIdx + 1)
      in searchBucket (wflDoubleMapEntries wfl) bitRack (fromIntegral start) (fromIntegral end)

-- | Linear search within a bucket for matching BitRack
searchBucket :: V.Vector WMPEntry -> BitRack -> Int -> Int -> Maybe WMPEntry
searchBucket entries targetBitRack start end = go start
  where
    go !i
      | i >= end = Nothing
      | otherwise =
          let entry = entries V.! i
              entryBitRack = wmpEntryReadBitRack entry
          in if bitRackEqual entryBitRack targetBitRack
               then Just entry
               else go (i + 1)

-- | Get word entry based on number of blanks in the BitRack
wmpGetWordEntry :: WMP -> BitRack -> Int -> Maybe WMPEntry
wmpGetWordEntry wmp bitRack wordLength
  | wordLength < 2 || wordLength > fromIntegral (wmpBoardDim wmp) = Nothing
  | otherwise =
      let wfl = wmpForLengths wmp V.! wordLength
          numBlanks = bitRackNumBlanks bitRack
      in case numBlanks of
           0 -> wflGetWordEntry wfl bitRack
           1 -> wflGetBlankEntry wfl bitRack
           2 -> wflGetDoubleBlankEntry wfl bitRack
           _ -> Nothing

-- | Write blankless words to a buffer (returns list of machine letter sequences)
-- Each sequence is a word that anagrams to the given BitRack
wmpWriteWordsToBuffer :: WMP -> BitRack -> Int -> [[MachineLetter]]
wmpWriteWordsToBuffer wmp bitRack wordLength
  | wordLength < 2 || wordLength > fromIntegral (wmpBoardDim wmp) = []
  | otherwise =
      let wfl = wmpForLengths wmp V.! wordLength
          numBlanks = bitRackNumBlanks bitRack
      in case numBlanks of
           0 -> wflWriteBlanklessWords wfl bitRack wordLength
           1 -> wflWriteBlanksWords wfl bitRack wordLength (MachineLetter 1)
           2 -> wflWriteDoubleBlanksWords wfl bitRack wordLength
           _ -> []

-- | Write blankless words from entry
wflWriteBlanklessWords :: WMPForLength -> BitRack -> Int -> [[MachineLetter]]
wflWriteBlanklessWords wfl bitRack wordLength =
  case wflGetWordEntry wfl bitRack of
    Nothing -> []
    Just entry -> entryWriteBlanklessWords entry wfl wordLength

-- | Extract words from a blankless entry
entryWriteBlanklessWords :: WMPEntry -> WMPForLength -> Int -> [[MachineLetter]]
entryWriteBlanklessWords entry wfl wordLength
  | wmpEntryIsInlined entry = extractInlinedWords entry wordLength
  | otherwise = extractUninlinedWords entry wfl wordLength

-- | Extract inlined words from entry
extractInlinedWords :: WMPEntry -> Int -> [[MachineLetter]]
extractInlinedWords entry wordLength =
  let bs = wmpEntryInlineOrPointer entry
      maxWords = wmpInlineValueBytes `div` wordLength
      extractWord offset
        | offset + wordLength > BS.length bs = Nothing
        | BS.index bs offset == 0 = Nothing  -- Zero terminator
        | otherwise = Just [ MachineLetter (BS.index bs (offset + i))
                           | i <- [0 .. wordLength - 1] ]
  in takeWhile (not . null) $ map (maybe [] id . extractWord) [i * wordLength | i <- [0 .. maxWords - 1]]

-- | Extract uninlined words from entry
extractUninlinedWords :: WMPEntry -> WMPForLength -> Int -> [[MachineLetter]]
extractUninlinedWords entry wfl wordLength =
  let start = fromIntegral $ wmpEntryWordStart entry
      numWords = fromIntegral $ wmpEntryNumWords entry
      letters = wflWordLetters wfl
      extractWord idx =
        let offset = (start + idx) * wordLength
        in [ MachineLetter (BS.index letters (offset + i))
           | i <- [0 .. wordLength - 1] ]
  in [ extractWord i | i <- [0 .. numWords - 1] ]

-- | Write words for single-blank BitRack
wflWriteBlanksWords :: WMPForLength -> BitRack -> Int -> MachineLetter -> [[MachineLetter]]
wflWriteBlanksWords wfl bitRack wordLength minMl =
  case wflGetBlankEntry wfl bitRack of
    Nothing -> []
    Just entry ->
      let blankLetters = wmpEntryBlankLetters entry
          -- Remove the blank, try each letter that has solutions
          bitRackNoBlank = bitRackRemoveLetter (MachineLetter 0) bitRack
      in concatMap (tryBlankLetter bitRackNoBlank blankLetters) [unML minMl .. 31]
  where
    tryBlankLetter br blankLetters ml
      | not (testBit blankLetters (fromIntegral ml)) = []
      | otherwise =
          let brWithLetter = bitRackAddLetter (MachineLetter ml) br
          in wflWriteBlanklessWords wfl brWithLetter wordLength

-- | Write words for double-blank BitRack
wflWriteDoubleBlanksWords :: WMPForLength -> BitRack -> Int -> [[MachineLetter]]
wflWriteDoubleBlanksWords wfl bitRack wordLength =
  case wflGetDoubleBlankEntry wfl bitRack of
    Nothing -> []
    Just entry ->
      let blankLetters = wmpEntryBlankLetters entry
          -- Remove one blank, iterate through first blank letters
          bitRackOneBlank = bitRackRemoveLetter (MachineLetter 0) bitRack
      in concatMap (tryFirstBlankLetter bitRackOneBlank blankLetters) [1 .. 31]
  where
    tryFirstBlankLetter br blankLetters ml
      | not (testBit blankLetters (fromIntegral ml)) = []
      | otherwise =
          let brWithLetter = bitRackAddLetter (MachineLetter ml) br
          in wflWriteBlanksWords wfl brWithLetter wordLength (MachineLetter ml)
