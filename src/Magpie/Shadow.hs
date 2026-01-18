{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Shadow algorithm for computing upper bounds on move scores
-- Ported from MAGPIE (domino14/magpie) - originally developed in wolges
-- See https://github.com/andy-k/wolges/blob/main/details.txt
--
-- The shadow algorithm is direction-agnostic: it uses a row cache that is
-- loaded from a lanes cache (pre-transposed board data for both directions).
-- This way, the same algorithm works for horizontal and vertical plays.
module Magpie.Shadow
  ( -- * Types
    Anchor(..)
  , AnchorHeap
  , ShadowConfig(..)
  , defaultShadowConfig
    -- * Shadow generation
  , generateAnchors
  , shadowPlayForAnchor
    -- * Testing helpers
  , extractSortedAnchors
  ) where

import Magpie.Types
import Magpie.KWG
import Magpie.Board (Board, computeAnchors, computeCrossSets, getCrossSet, getCrossScore,
                     getSquare, getLetter, isEmpty, isAnchor, getTilesPlayed,
                     trivialCrossSet, getLeftExtensionSet, getRightExtensionSet)
import Magpie.LetterDistribution
import Magpie.WMPMoveGen

import Data.Word (Word64)
import Data.Bits ((.&.), (.|.), complement, popCount, countTrailingZeros)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Vector.Unboxed as VU
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed.Mutable as MVU

-- | An anchor represents a position where moves can start, with upper bound info
data Anchor = Anchor
  { anchorRow                  :: !Int
  , anchorCol                  :: !Int
  , anchorLastAnchorCol        :: !Int
  , anchorDir                  :: !Direction
  , anchorHighestPossibleScore :: !Int  -- Upper bound on score
  , anchorHighestPossibleEquity :: !Int -- Upper bound on equity (score + leave)
  , anchorTilesToPlay          :: !Int  -- For WMP
  , anchorPlaythroughBlocks    :: !Int  -- For WMP
  , anchorWordLength           :: !Int  -- For WMP
  , anchorLeftmostStartCol     :: !Int  -- For WMP
  , anchorRightmostStartCol    :: !Int  -- For WMP
  } deriving (Show, Eq)

-- | Anchor heap (list for now, could optimize to actual heap later)
type AnchorHeap = [Anchor]

-- | Configuration for shadow generation
data ShadowConfig = ShadowConfig
  { shadowBingoBonus :: !Int
  , shadowSortByScore :: !Bool  -- If True, sort by score; if False, by equity
  } deriving (Show)

-- | Default shadow config
defaultShadowConfig :: ShadowConfig
defaultShadowConfig = ShadowConfig
  { shadowBingoBonus = 50
  , shadowSortByScore = True
  }

-- | Unrestricted multiplier with column information
-- Used to track multipliers that need recalculation when word_multiplier changes
data UnrestrictedMul = UnrestrictedMul
  { umMultiplier :: !Int
  , umColumn     :: !Int
  } deriving (Show)

-- | Move generation state - matches MAGPIE MoveGen structure
-- This struct is used for both move generation and shadow scoring
data MoveGen = MoveGen
  { -- Position tracking
    mgCurrentRowIndex        :: !Int
  , mgCurrentAnchorCol       :: !Int
  , mgAnchorLeftExtensionSet :: !Word64
  , mgAnchorRightExtensionSet :: !Word64
  , mgLastAnchorCol          :: !Int
  , mgDir                    :: !Direction

    -- Play tracking
  , mgMaxTilesToPlay         :: !Int
  , mgTilesPlayed            :: !Int
  , mgBingoBonus             :: !Int

    -- Rack state
  , mgPlayerRack             :: !Rack
  , mgRackCrossSet           :: !Word64
  , mgNumberOfLettersOnRack  :: !Int

    -- Shadow play state (left/right extension tracking)
  , mgCurrentLeftCol         :: !Int
  , mgCurrentRightCol        :: !Int

    -- Unrestricted multipliers for shadow scoring
    -- Stored descending by effective multiplier
  , mgDescCrossWordMuls      :: ![UnrestrictedMul]  -- Cross-word multipliers with columns
  , mgDescEffLetterMuls      :: ![Int]              -- Effective letter multipliers (descending)
  , mgNumUnrestrictedMuls    :: !Int
  , mgLastWordMultiplier     :: !Int                -- For recalculation detection

    -- Shadow scoring accumulators
  , mgShadowPerpAdditionalScore    :: !Int  -- Cross-word scores
  , mgShadowMainwordRestrictedScore :: !Int  -- Playthrough + restricted tiles * letter_mult
  , mgShadowWordMultiplier         :: !Int  -- Product of word multipliers

    -- Best scores found
  , mgHighestShadowEquity    :: !Int
  , mgHighestShadowScore     :: !Int

    -- Descending tile scores for optimal assignment
  , mgFullRackDescTileScores :: !(VU.Vector Int)
  , mgDescTileScores         :: !(VU.Vector Int)

    -- Row cache - the current row being processed
    -- This is loaded from lanes_cache and provides direction-agnostic access
  , mgRowCache               :: ![Square]  -- BOARD_DIM squares

    -- Letter distribution reference
  , mgLdSize                 :: !Int

    -- WMP move generation state
  , mgWMPMoveGen             :: !WMPMoveGen
  } deriving (Show)

-- | Initialize MoveGen for shadow scoring
initMoveGen :: LetterDistribution -> Rack -> Int -> Int -> WMPMoveGen -> MoveGen
initMoveGen ld rack bingoBonus anchorCol wmg =
  let rackCS = computeRackCrossSet rack (ldSize ld)
      numLetters = rackTotal rack
      descScores = sortTileScoresDescending ld rack
  in MoveGen
     { mgCurrentRowIndex = 0
     , mgCurrentAnchorCol = anchorCol
     , mgAnchorLeftExtensionSet = trivialCrossSet (ldSize ld)
     , mgAnchorRightExtensionSet = trivialCrossSet (ldSize ld)
     , mgLastAnchorCol = -1
     , mgDir = Horizontal
     , mgMaxTilesToPlay = 0
     , mgTilesPlayed = 0
     , mgBingoBonus = bingoBonus
     , mgPlayerRack = rack
     , mgRackCrossSet = rackCS
     , mgNumberOfLettersOnRack = numLetters
     , mgCurrentLeftCol = anchorCol
     , mgCurrentRightCol = anchorCol
     , mgDescCrossWordMuls = []
     , mgDescEffLetterMuls = []
     , mgNumUnrestrictedMuls = 0
     , mgLastWordMultiplier = 1
     , mgShadowPerpAdditionalScore = 0
     , mgShadowMainwordRestrictedScore = 0
     , mgShadowWordMultiplier = 1
     , mgHighestShadowEquity = 0
     , mgHighestShadowScore = 0
     , mgFullRackDescTileScores = descScores
     , mgDescTileScores = descScores
     , mgRowCache = []
     , mgLdSize = ldSize ld
     , mgWMPMoveGen = wmg
     }

-- | Compute rack cross set (bitmask of available letters)
computeRackCrossSet :: Rack -> Int -> Word64
computeRackCrossSet rack distSize =
  let hasBlank = rackHasLetter (MachineLetter 0) rack
      allLetters = foldr (\i acc -> acc .|. (1 `shiftL` i)) 0 [1 .. distSize - 1]
  in if hasBlank
     then allLetters
     else foldr (\i acc ->
            if rackHasLetter (MachineLetter (fromIntegral i)) rack
            then acc .|. (1 `shiftL` i)
            else acc) 0 [1 .. distSize - 1]
  where
    shiftL x n = x * (2 ^ n)

-- | Sort tile scores in descending order based on rack contents
sortTileScoresDescending :: LetterDistribution -> Rack -> VU.Vector Int
sortTileScoresDescending ld rack = runST $ do
  let size = ldSize ld
      numLetters = rackTotal rack
  scores <- MVU.new numLetters
  let go !idx !ml
        | ml >= size = return ()
        | otherwise = do
            let count = rackGetCount (MachineLetter (fromIntegral ml)) rack
                score = ldScore ld (MachineLetter (fromIntegral ml))
            let addScores !i !n
                  | n <= 0 = return i
                  | otherwise = do
                      MVU.write scores i score
                      addScores (i + 1) (n - 1)
            idx' <- addScores idx count
            go idx' (ml + 1)
  go 0 0
  vec <- VU.freeze scores
  return $ VU.fromList $ sortBy (comparing Down) $ VU.toList vec

-- | Generate anchors with shadow scores for a board position
generateAnchors :: ShadowConfig -> KWG -> LetterDistribution -> Board -> Rack -> WMPMoveGen -> AnchorHeap
generateAnchors cfg kwg ld board rack wmg =
  let -- Prepare board with cross-sets and anchors for both directions
      boardH = computeAnchors $ computeCrossSets kwg ld Horizontal board
      boardV = computeAnchors $ computeCrossSets kwg ld Vertical board

      -- Generate anchors for horizontal direction
      hAnchors = concatMap (shadowRow cfg ld boardH rack Horizontal wmg) [0..boardDim-1]

      -- Generate anchors for vertical direction (skip if empty board)
      boardIsEmpty = getTilesPlayed board == 0
      vAnchors = if boardIsEmpty
                 then []
                 else concatMap (shadowRow cfg ld boardV rack Vertical wmg) [0..boardDim-1]

      allAnchors = hAnchors ++ vAnchors
  in sortBy (comparing (Down . anchorHighestPossibleEquity)) allAnchors

-- | Generate anchors for a single row (direction-agnostic)
-- For horizontal plays, this processes a row (row index, iterating over columns)
-- For vertical plays, the board is conceptually transposed, so this still
-- processes a "row" which is actually a column in the original board
shadowRow :: ShadowConfig -> LetterDistribution -> Board -> Rack -> Direction -> WMPMoveGen -> Int -> AnchorHeap
shadowRow cfg ld board rack dir wmg rowOrCol =
  let -- Find anchors in this row/column
      anchors = [c | c <- [0..boardDim-1], isAnchorForDir board rowOrCol c dir]
  in processAnchors cfg ld board rack dir wmg rowOrCol (-1) anchors

-- | Check if a position is an anchor for a given direction
isAnchorForDir :: Board -> Int -> Int -> Direction -> Bool
isAnchorForDir board rowOrCol col dir =
  case dir of
    Horizontal -> isAnchor board rowOrCol col
    Vertical   -> isAnchor board col rowOrCol  -- Swap for vertical

-- | Get board data for a position based on direction
getBoardData :: Board -> Int -> Int -> Direction -> (MachineLetter, Word64, Int, BonusSquare, Word64, Word64)
getBoardData board rowOrCol col dir =
  let (r, c) = case dir of
                 Horizontal -> (rowOrCol, col)
                 Vertical   -> (col, rowOrCol)
      sq = getSquare board r c
      letter = getLetter board r c
      crossSet = getCrossSet board r c
      crossScore = getCrossScore board r c
      bonus = sqBonus sq
      leftExt = getLeftExtensionSet board r c
      rightExt = getRightExtensionSet board r c
  in (letter, crossSet, crossScore, bonus, leftExt, rightExt)

-- | Check if position is empty based on direction
isEmptyForDir :: Board -> Int -> Int -> Direction -> Bool
isEmptyForDir board rowOrCol col dir =
  case dir of
    Horizontal -> isEmpty board rowOrCol col
    Vertical   -> isEmpty board col rowOrCol

-- | Get letter for position based on direction
getLetterForDir :: Board -> Int -> Int -> Direction -> MachineLetter
getLetterForDir board rowOrCol col dir =
  case dir of
    Horizontal -> getLetter board rowOrCol col
    Vertical   -> getLetter board col rowOrCol

-- | Process anchors for shadow generation
processAnchors :: ShadowConfig -> LetterDistribution -> Board -> Rack -> Direction
               -> WMPMoveGen -> Int -> Int -> [Int] -> AnchorHeap
processAnchors _ _ _ _ _ _ _ _ [] = []
processAnchors cfg ld board rack dir wmg rowOrCol lastAnchorCol (col:rest) =
  let -- Reset WMP playthrough state for each anchor
      wmg' = wmpMoveGenResetPlaythrough wmg
      mAnchor = shadowPlayForAnchor cfg ld board rack dir wmg' rowOrCol col lastAnchorCol
      -- Update lastAnchorCol
      newLastAnchorCol = if not (isEmptyForDir board rowOrCol col dir)
                         then col + 1  -- Skip one after occupied
                         else col
      restAnchors = processAnchors cfg ld board rack dir wmg rowOrCol newLastAnchorCol rest
  in case mAnchor of
       Nothing -> restAnchors
       Just anchor -> anchor : restAnchors

-- | Compute shadow score for a single anchor
-- Returns Nothing if no valid plays exist from this anchor
shadowPlayForAnchor :: ShadowConfig -> LetterDistribution -> Board -> Rack -> Direction
                    -> WMPMoveGen -> Int -> Int -> Int -> Maybe Anchor
shadowPlayForAnchor cfg ld board rack dir wmg rowOrCol col lastAnchorCol =
  let gen0 = initMoveGen ld rack (shadowBingoBonus cfg) col wmg
      gen1 = gen0 { mgLastAnchorCol = lastAnchorCol
                  , mgDir = dir
                  , mgCurrentRowIndex = rowOrCol
                  }

      -- Get extension sets for this position
      (_, _, _, _, leftExt, rightExt) = getBoardData board rowOrCol col dir
      gen2 = gen1 { mgAnchorLeftExtensionSet = if leftExt == 0 then trivialCrossSet (ldSize ld) else leftExt
                  , mgAnchorRightExtensionSet = if rightExt == 0 then trivialCrossSet (ldSize ld) else rightExt
                  }

      -- Check if any extension possible
      anyExtension = mgAnchorLeftExtensionSet gen2 /= 0 || mgAnchorRightExtensionSet gen2 /= 0

  in if not anyExtension
     then Nothing
     else
       let currentLetter = getLetterForDir board rowOrCol col dir
           gen3 = if unML currentLetter == 0
                  then shadowStartNonplaythrough cfg ld board gen2
                  else shadowStartPlaythrough cfg ld board currentLetter gen2

           maxTiles = mgMaxTilesToPlay gen3
           (actualRow, actualCol) = case dir of
                                      Horizontal -> (rowOrCol, col)
                                      Vertical   -> (col, rowOrCol)
       in if maxTiles == 0
          then Nothing
          else Just Anchor
               { anchorRow = actualRow
               , anchorCol = actualCol
               , anchorLastAnchorCol = lastAnchorCol
               , anchorDir = dir
               , anchorHighestPossibleScore = mgHighestShadowScore gen3
               , anchorHighestPossibleEquity = mgHighestShadowEquity gen3
               , anchorTilesToPlay = maxTiles
               , anchorPlaythroughBlocks = 0
               , anchorWordLength = 0
               , anchorLeftmostStartCol = col
               , anchorRightmostStartCol = col
               }

-- | Start shadow from empty anchor square (non-playthrough)
shadowStartNonplaythrough :: ShadowConfig -> LetterDistribution -> Board -> MoveGen -> MoveGen
shadowStartNonplaythrough cfg ld board gen =
  let col = mgCurrentLeftCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen
      (_, crossSet, crossScore, bonus, _, _) = getBoardData board rowOrCol col dir
      rackCS = mgRackCrossSet gen
      possibleLetters = crossSet .&. rackCS

  in if possibleLetters == 0
     then gen
     else
       let letterMult = letterMultiplier bonus
           thisWordMult = wordMultiplier bonus

           -- Set word multiplier to 0 temporarily (like C code)
           gen0 = gen { mgShadowWordMultiplier = 0 }

           -- Try to restrict tile if only one letter possible
           (restricted, restrictedTileScore, gen1) = tryRestrictTile ld possibleLetters letterMult gen0

           -- Insert unrestricted multiplier BEFORE recording
           crossWordMult = if crossScore > 0 then letterMult * thisWordMult else 0
           gen2 = if not restricted
                  then insertUnrestrictedMultipliers ld letterMult thisWordMult crossWordMult col gen1
                  else gen1

           -- Compute perpendicular score contribution
           -- For restricted tiles with cross-words, include the tile's contribution
           perpScore = crossScore * thisWordMult +
                       (if restricted && crossScore > 0
                        then restrictedTileScore * letterMult * thisWordMult
                        else 0)

           -- Update tiles played and perp score
           gen3 = gen2
             { mgTilesPlayed = mgTilesPlayed gen2 + 1
             , mgShadowPerpAdditionalScore = perpScore
             }

           -- Record single tile play (for both directions)
           gen4 = shadowRecord cfg gen3

           -- Set word multiplier to actual value
           gen5 = gen4 { mgShadowWordMultiplier = thisWordMult }

           -- Recalculate effective multipliers with correct word_mult
           gen6 = maybeRecalculateEffectiveMultipliers ld gen5

           -- Continue left
           isUnique = dir == Horizontal
           gen7 = nonplaythroughShadowPlayLeft cfg ld board isUnique gen6

       in gen7

-- | Start shadow from occupied anchor square (playthrough)
shadowStartPlaythrough :: ShadowConfig -> LetterDistribution -> Board -> MachineLetter -> MoveGen -> MoveGen
shadowStartPlaythrough cfg ld board currentLetter gen =
  let -- Traverse through all placed tiles
      gen1 = traversePlaythrough ld board currentLetter gen
      -- Increment playthrough blocks in WMP state
      wmg2 = if wmpMoveGenIsActive (mgWMPMoveGen gen1)
             then wmpMoveGenIncrementPlaythroughBlocks (mgWMPMoveGen gen1)
             else mgWMPMoveGen gen1
      gen2 = gen1 { mgWMPMoveGen = wmg2 }
      -- Continue with playthrough shadow
      isUnique = mgDir gen == Horizontal
      gen3 = playthroughShadowPlayLeft cfg ld board isUnique gen2
  in gen3

-- | Traverse through placed tiles going left, accumulating score
-- Also tracks playthrough letters in WMP state
traversePlaythrough :: LetterDistribution -> Board -> MachineLetter -> MoveGen -> MoveGen
traversePlaythrough ld board ml gen =
  let unblanked = unblankLetter ml
      tileScore = ldScore ld unblanked
      col = mgCurrentLeftCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen
      lastAnchorCol = mgLastAnchorCol gen
      -- Add playthrough letter to WMP state
      wmg1 = if wmpMoveGenIsActive (mgWMPMoveGen gen)
             then wmpMoveGenAddPlaythroughLetter unblanked (mgWMPMoveGen gen)
             else mgWMPMoveGen gen
      gen1 = gen { mgShadowMainwordRestrictedScore = mgShadowMainwordRestrictedScore gen + tileScore
                 , mgWMPMoveGen = wmg1
                 }
  in if col == 0 || col == lastAnchorCol + 1
     then gen1
     else
       let newCol = col - 1
           nextLetter = getLetterForDir board rowOrCol newCol dir
       in if unML nextLetter == 0
          then gen1 { mgCurrentLeftCol = col }  -- Hit empty, stay at current
          else traversePlaythrough ld board nextLetter
                                   (gen1 { mgCurrentLeftCol = newCol })

-- | Shadow play left from non-playthrough start
nonplaythroughShadowPlayLeft :: ShadowConfig -> LetterDistribution -> Board -> Bool -> MoveGen -> MoveGen
nonplaythroughShadowPlayLeft cfg ld board isUnique gen =
  -- First try extending right
  let possibleRight = mgAnchorRightExtensionSet gen .&. mgRackCrossSet gen
      gen1 = if possibleRight /= 0
             then shadowPlayRight cfg ld board isUnique gen
             else gen
      gen2 = gen1 { mgAnchorRightExtensionSet = trivialCrossSet (mgLdSize gen) }

      col = mgCurrentLeftCol gen2
      rowOrCol = mgCurrentRowIndex gen2
      dir = mgDir gen2
      lastAnchorCol = mgLastAnchorCol gen2
  in if col == 0 || col == lastAnchorCol + 1 ||
        mgTilesPlayed gen2 >= mgNumberOfLettersOnRack gen2
     then gen2
     else
       -- Check if left square is occupied (playthrough)
       let leftLetter = if col > 0 then getLetterForDir board rowOrCol (col - 1) dir else MachineLetter 0
       in if unML leftLetter /= 0
          then -- Found playthrough tile, traverse through it
            -- After traversing playthrough, allow any extension (reset extension set)
            let gen3 = traverseLeftPlaythrough ld board gen2
                -- Increment playthrough blocks in WMP state
                wmg4 = if wmpMoveGenIsActive (mgWMPMoveGen gen3)
                       then wmpMoveGenIncrementPlaythroughBlocks (mgWMPMoveGen gen3)
                       else mgWMPMoveGen gen3
                gen4 = gen3 { mgAnchorLeftExtensionSet = trivialCrossSet (mgLdSize gen)
                            , mgWMPMoveGen = wmg4
                            }
            in nonplaythroughShadowPlayLeft cfg ld board isUnique gen4
          else
            -- Empty square, check if we can extend
            let possibleLeft = mgAnchorLeftExtensionSet gen2 .&. mgRackCrossSet gen2
            in if possibleLeft == 0
               then gen2
               else
                 let (_, crossSet, crossScore, bonus, _, _) = getBoardData board rowOrCol (col - 1) dir
                     possibleHere = possibleLeft .&. crossSet
                 in if possibleHere == 0
                    then gen2
                    else
                      let gen3 = gen2 { mgAnchorLeftExtensionSet = trivialCrossSet (mgLdSize gen2)
                                      , mgCurrentLeftCol = col - 1
                                      , mgTilesPlayed = mgTilesPlayed gen2 + 1
                                      }
                          letterMult = letterMultiplier bonus
                          thisWordMult = wordMultiplier bonus

                          (restricted, restrictedTileScore, gen4) = tryRestrictTile ld possibleHere letterMult gen3

                          -- Compute perpendicular score contribution
                          -- For restricted tiles with cross-words, include the tile's contribution
                          perpContrib = crossScore * thisWordMult +
                                        (if restricted && crossScore > 0
                                         then restrictedTileScore * letterMult * thisWordMult
                                         else 0)

                          gen5 = gen4 { mgShadowWordMultiplier = mgShadowWordMultiplier gen4 * thisWordMult
                                      , mgShadowPerpAdditionalScore = mgShadowPerpAdditionalScore gen4 + perpContrib
                                      }

                          crossWordMult = if crossScore > 0 then letterMult * thisWordMult else 0
                          gen6 = if not restricted
                                 then insertUnrestrictedMultipliers ld letterMult thisWordMult crossWordMult (col - 1) gen5
                                 else gen5

                          gen7 = shadowRecord cfg gen6

                      in nonplaythroughShadowPlayLeft cfg ld board isUnique gen7

-- | Traverse through playthrough tiles going left
-- Also tracks playthrough letters in WMP state
traverseLeftPlaythrough :: LetterDistribution -> Board -> MoveGen -> MoveGen
traverseLeftPlaythrough ld board gen =
  let col = mgCurrentLeftCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen
      lastAnchorCol = mgLastAnchorCol gen
  in if col == 0 || col == lastAnchorCol + 1
     then gen
     else
       let leftCol = col - 1
           leftLetter = getLetterForDir board rowOrCol leftCol dir
       in if unML leftLetter == 0
          then gen  -- Hit empty, stay at current
          else
            let unblanked = unblankLetter leftLetter
                tileScore = ldScore ld unblanked
                -- Add playthrough letter to WMP state
                wmg1 = if wmpMoveGenIsActive (mgWMPMoveGen gen)
                       then wmpMoveGenAddPlaythroughLetter unblanked (mgWMPMoveGen gen)
                       else mgWMPMoveGen gen
                gen1 = gen { mgShadowMainwordRestrictedScore = mgShadowMainwordRestrictedScore gen + tileScore
                           , mgCurrentLeftCol = leftCol
                           , mgWMPMoveGen = wmg1
                           }
            in traverseLeftPlaythrough ld board gen1

-- | Shadow play left from playthrough start
playthroughShadowPlayLeft :: ShadowConfig -> LetterDistribution -> Board -> Bool -> MoveGen -> MoveGen
playthroughShadowPlayLeft cfg ld board isUnique gen =
  -- First try extending right
  let possibleRight = mgAnchorRightExtensionSet gen .&. mgRackCrossSet gen
      gen1 = if possibleRight /= 0
             then shadowPlayRight cfg ld board isUnique gen
             else gen
      gen2 = gen1 { mgAnchorRightExtensionSet = trivialCrossSet (mgLdSize gen) }

      col = mgCurrentLeftCol gen2
      rowOrCol = mgCurrentRowIndex gen2
      dir = mgDir gen2
      lastAnchorCol = mgLastAnchorCol gen2
  in if col == 0 || col == lastAnchorCol + 1 ||
        mgTilesPlayed gen2 >= mgNumberOfLettersOnRack gen2
     then gen2
     else
       -- Check if left square is occupied (playthrough)
       let leftLetter = if col > 0 then getLetterForDir board rowOrCol (col - 1) dir else MachineLetter 0
       in if unML leftLetter /= 0
          then -- Found playthrough tile, traverse through it
            -- After traversing playthrough, allow any extension (reset extension set)
            let gen3 = traverseLeftPlaythrough ld board gen2
                -- Increment playthrough blocks in WMP state
                wmg4 = if wmpMoveGenIsActive (mgWMPMoveGen gen3)
                       then wmpMoveGenIncrementPlaythroughBlocks (mgWMPMoveGen gen3)
                       else mgWMPMoveGen gen3
                gen4 = gen3 { mgAnchorLeftExtensionSet = trivialCrossSet (mgLdSize gen)
                            , mgWMPMoveGen = wmg4
                            }
            in playthroughShadowPlayLeft cfg ld board isUnique gen4
          else
            -- Empty square, check if we can extend
            let possibleLeft = mgAnchorLeftExtensionSet gen2 .&. mgRackCrossSet gen2
            in if possibleLeft == 0
               then gen2
               else
                 let (_, crossSet, crossScore, bonus, _, _) = getBoardData board rowOrCol (col - 1) dir
                     possibleHere = possibleLeft .&. crossSet
                 in if possibleHere == 0
                    then gen2
                    else
                      let gen3 = gen2 { mgAnchorLeftExtensionSet = trivialCrossSet (mgLdSize gen2)
                                      , mgCurrentLeftCol = col - 1
                                      , mgTilesPlayed = mgTilesPlayed gen2 + 1
                                      }
                          letterMult = letterMultiplier bonus
                          thisWordMult = wordMultiplier bonus

                          (restricted, restrictedTileScore, gen4) = tryRestrictTile ld possibleHere letterMult gen3

                          -- Compute perpendicular score contribution
                          -- For restricted tiles with cross-words, include the tile's contribution
                          perpContrib = crossScore * thisWordMult +
                                        (if restricted && crossScore > 0
                                         then restrictedTileScore * letterMult * thisWordMult
                                         else 0)

                          gen5 = gen4
                            { mgShadowPerpAdditionalScore = mgShadowPerpAdditionalScore gen4 + perpContrib
                            , mgShadowWordMultiplier = mgShadowWordMultiplier gen4 * thisWordMult
                            }

                          crossWordMult = if crossScore > 0 then letterMult * thisWordMult else 0
                          gen6 = if not restricted
                                 then insertUnrestrictedMultipliers ld letterMult thisWordMult crossWordMult (col - 1) gen5
                                 else gen5

                          newUnique = if crossSet == trivialCrossSet (mgLdSize gen) then True else isUnique

                          gen7 = if playIsNonemptyAndNonduplicate (mgTilesPlayed gen6) newUnique
                                 then shadowRecord cfg gen6
                                 else gen6

                      in playthroughShadowPlayLeft cfg ld board newUnique gen7

-- | Shadow play to the right
shadowPlayRight :: ShadowConfig -> LetterDistribution -> Board -> Bool -> MoveGen -> MoveGen
shadowPlayRight cfg ld board isUnique gen0 =
  -- Save original state for restoration
  let origMainScore = mgShadowMainwordRestrictedScore gen0
      origPerpScore = mgShadowPerpAdditionalScore gen0
      origWordMult = mgShadowWordMultiplier gen0
      origRightCol = mgCurrentRightCol gen0
      origTilesPlayed = mgTilesPlayed gen0
      origDescScores = mgDescTileScores gen0
      origRack = mgPlayerRack gen0
      origRackCS = mgRackCrossSet gen0
      origDescXWMuls = mgDescCrossWordMuls gen0
      origDescEffMuls = mgDescEffLetterMuls gen0
      origNumUnrestrMuls = mgNumUnrestrictedMuls gen0

      -- Save WMP playthrough state before extending right
      wmgSaved = wmpMoveGenSavePlaythroughState (mgWMPMoveGen gen0)
      gen0' = gen0 { mgWMPMoveGen = wmgSaved }

      -- Extend right
      (finalGen, _) = extendRight cfg ld board isUnique gen0'

      -- Restore WMP playthrough state
      wmgRestored = wmpMoveGenRestorePlaythroughState (mgWMPMoveGen finalGen)

      -- Restore original state but keep highest scores
      restoredGen = finalGen
        { mgShadowMainwordRestrictedScore = origMainScore
        , mgShadowPerpAdditionalScore = origPerpScore
        , mgShadowWordMultiplier = origWordMult
        , mgCurrentRightCol = origRightCol
        , mgTilesPlayed = origTilesPlayed
        , mgDescTileScores = origDescScores
        , mgPlayerRack = origRack
        , mgRackCrossSet = origRackCS
        , mgDescCrossWordMuls = origDescXWMuls
        , mgDescEffLetterMuls = origDescEffMuls
        , mgNumUnrestrictedMuls = origNumUnrestrMuls
        , mgWMPMoveGen = wmgRestored
        }
  in restoredGen

-- | Extend shadow to the right
extendRight :: ShadowConfig -> LetterDistribution -> Board -> Bool -> MoveGen -> (MoveGen, Bool)
extendRight cfg ld board isUnique gen =
  let col = mgCurrentRightCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen
  in if col >= boardDim - 1 || mgTilesPlayed gen >= mgNumberOfLettersOnRack gen
     then (gen, isUnique)
     else
       let newCol = col + 1
           -- Check if next position is occupied (playthrough)
           nextLetter = getLetterForDir board rowOrCol newCol dir
       in if unML nextLetter /= 0
          then -- Occupied, traverse through playthrough tiles
            let (gen1, foundPlaythrough) = traverseRightPlaythrough ld board
                                             (gen { mgCurrentRightCol = col })  -- Start from current col
                -- Increment playthrough blocks in WMP state if we found any
                wmg2 = if foundPlaythrough && wmpMoveGenIsActive (mgWMPMoveGen gen1)
                       then wmpMoveGenIncrementPlaythroughBlocks (mgWMPMoveGen gen1)
                       else mgWMPMoveGen gen1
                gen2 = gen1 { mgAnchorRightExtensionSet = trivialCrossSet (mgLdSize gen)
                            , mgWMPMoveGen = wmg2
                            }
            in extendRight cfg ld board isUnique gen2
          else
            -- Empty square, try to place a tile
            let (_, crossSet, crossScore, bonus, leftExt, _) = getBoardData board rowOrCol newCol dir
                rackCS = mgRackCrossSet gen
                rightExt = mgAnchorRightExtensionSet gen

                -- Non-blank cross set and left extension
                nonblankCS = crossSet .&. complement 1
                nonblankLeftExt = leftExt .&. complement 1

            in if (nonblankCS .&. nonblankLeftExt) == 0
               then (gen, isUnique)
               else
                 let possibleHere = crossSet .&. rackCS .&. rightExt .&. leftExt
                 in if possibleHere == 0
                    then (gen, isUnique)
                    else
                 let gen1 = gen
                       { mgCurrentRightCol = newCol
                       , mgTilesPlayed = mgTilesPlayed gen + 1
                       , mgAnchorRightExtensionSet = trivialCrossSet (mgLdSize gen)
                       }

                     letterMult = letterMultiplier bonus
                     thisWordMult = wordMultiplier bonus

                     (restricted, restrictedTileScore, gen2) = tryRestrictTile ld possibleHere letterMult gen1

                     -- Compute perpendicular score contribution
                     -- For restricted tiles with cross-words, include the tile's contribution
                     perpContrib = crossScore * thisWordMult +
                                   (if restricted && crossScore > 0
                                    then restrictedTileScore * letterMult * thisWordMult
                                    else 0)

                     gen3 = gen2
                       { mgShadowPerpAdditionalScore = mgShadowPerpAdditionalScore gen2 + perpContrib
                       , mgShadowWordMultiplier = mgShadowWordMultiplier gen2 * thisWordMult
                       }

                     crossWordMult = if crossScore > 0 then letterMult * thisWordMult else 0
                     gen4 = if not restricted
                            then insertUnrestrictedMultipliers ld letterMult thisWordMult crossWordMult newCol gen3
                            else gen3

                     newUnique = if crossSet == trivialCrossSet (mgLdSize gen) then True else isUnique

                     -- Traverse through any playthrough tiles
                     (gen5, foundPlaythrough2) = traverseRightPlaythrough ld board gen4
                     -- Increment playthrough blocks in WMP state if we found any
                     wmg5 = if foundPlaythrough2 && wmpMoveGenIsActive (mgWMPMoveGen gen5)
                            then wmpMoveGenIncrementPlaythroughBlocks (mgWMPMoveGen gen5)
                            else mgWMPMoveGen gen5
                     gen5' = gen5 { mgWMPMoveGen = wmg5 }

                     gen6 = if playIsNonemptyAndNonduplicate (mgTilesPlayed gen5') newUnique
                            then shadowRecord cfg gen5'
                            else gen5'

                 in extendRight cfg ld board newUnique gen6

-- | Traverse through playthrough tiles to the right
-- Also tracks playthrough letters in WMP state and returns whether any were found
traverseRightPlaythrough :: LetterDistribution -> Board -> MoveGen -> (MoveGen, Bool)
traverseRightPlaythrough ld board gen =
  let col = mgCurrentRightCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen
  in if col + 1 >= boardDim
     then (gen, False)
     else
       let nextLetter = getLetterForDir board rowOrCol (col + 1) dir
       in if unML nextLetter == 0
          then (gen, False)
          else
            let unblanked = unblankLetter nextLetter
                tileScore = ldScore ld unblanked
                -- Add playthrough letter to WMP state
                wmg1 = if wmpMoveGenIsActive (mgWMPMoveGen gen)
                       then wmpMoveGenAddPlaythroughLetter unblanked (mgWMPMoveGen gen)
                       else mgWMPMoveGen gen
                gen1 = gen
                  { mgCurrentRightCol = col + 1
                  , mgShadowMainwordRestrictedScore = mgShadowMainwordRestrictedScore gen + tileScore
                  , mgWMPMoveGen = wmg1
                  }
                (gen2, _) = traverseRightPlaythrough ld board gen1
            in (gen2, True)

-- | Try to restrict a tile at a position (when cross-set has exactly one letter)
-- Returns (wasRestricted, tileScore, newGen)
-- The caller must add the restricted tile's contribution to cross-words if applicable
tryRestrictTile :: LetterDistribution -> Word64 -> Int -> MoveGen -> (Bool, Int, MoveGen)
tryRestrictTile ld possibleLetters letterMult gen =
  let isSingleBit = popCount possibleLetters == 1
  in if not isSingleBit
     then (False, 0, gen)
     else
       let ml = MachineLetter (fromIntegral (countTrailingZeros possibleLetters))
           tileScore = ldScore ld ml
           scoredLetter = tileScore * letterMult

           -- Remove from descending scores
           newDescScores = removeScoreFromDescending tileScore (mgDescTileScores gen)

           -- Update rack cross set
           newRack = rackTakeLetter ml (mgPlayerRack gen)
           newRackCS = if rackGetCount ml newRack == 0
                       then mgRackCrossSet gen .&. complement (1 `shiftL` fromIntegral (unML ml))
                       else mgRackCrossSet gen

           newGen = gen
             { mgShadowMainwordRestrictedScore = mgShadowMainwordRestrictedScore gen + scoredLetter
             , mgDescTileScores = newDescScores
             , mgPlayerRack = newRack
             , mgRackCrossSet = newRackCS
             }
       in (True, tileScore, newGen)
  where
    shiftL x n = x * (2 ^ n)

-- | Remove a score from the descending scores vector
removeScoreFromDescending :: Int -> VU.Vector Int -> VU.Vector Int
removeScoreFromDescending score vec =
  let list = VU.toList vec
      removeFirst _ [] = []
      removeFirst s (x:xs) = if x == s then xs else x : removeFirst s xs
  in VU.fromList (removeFirst score list)

-- | Insert unrestricted multipliers (cross-word and effective letter multipliers)
-- This matches the C insert_unrestricted_multipliers function
insertUnrestrictedMultipliers :: LetterDistribution -> Int -> Int -> Int -> Int -> MoveGen -> MoveGen
insertUnrestrictedMultipliers ld letterMult wordMult crossWordMult col gen =
  let -- Recalculate existing multipliers if word_mult changed
      gen1 = maybeRecalculateEffectiveMultipliers ld gen

      -- Insert cross-word multiplier (for recalculation later)
      newXWMul = UnrestrictedMul { umMultiplier = crossWordMult, umColumn = col }
      newXWMuls = insertSortedXWMul newXWMul (mgDescCrossWordMuls gen1)

      -- Calculate and insert effective letter multiplier
      -- effective = word_multiplier * letter_multiplier + cross_word_multiplier
      effMul = mgShadowWordMultiplier gen1 * letterMult + crossWordMult
      newEffMuls = insertSortedMul effMul (mgDescEffLetterMuls gen1)

  in gen1 { mgDescCrossWordMuls = newXWMuls
          , mgDescEffLetterMuls = newEffMuls
          , mgNumUnrestrictedMuls = mgNumUnrestrictedMuls gen1 + 1
          }
  where
    insertSortedXWMul x [] = [x]
    insertSortedXWMul x (y:ys)
      | umMultiplier x >= umMultiplier y = x : y : ys
      | otherwise = y : insertSortedXWMul x ys

    insertSortedMul x [] = [x]
    insertSortedMul x (y:ys)
      | x >= y = x : y : ys
      | otherwise = y : insertSortedMul x ys

-- | Recalculate effective multipliers when word multiplier changes
maybeRecalculateEffectiveMultipliers :: LetterDistribution -> MoveGen -> MoveGen
maybeRecalculateEffectiveMultipliers _ gen =
  if mgShadowWordMultiplier gen == mgLastWordMultiplier gen
  then gen
  else
    -- Recalculate all effective multipliers with new word_mult
    let xwMuls = mgDescCrossWordMuls gen
        newEffMuls = sortBy (comparing Down)
                     [mgShadowWordMultiplier gen * 1 + umMultiplier xw | xw <- xwMuls]
                     -- Note: letter_mult is assumed to be 1 here for simplicity
                     -- Full implementation would store letter_mult per column
    in gen { mgDescEffLetterMuls = newEffMuls
           , mgLastWordMultiplier = mgShadowWordMultiplier gen
           }

-- | Record a shadow score (compute upper bound and update if better)
-- Checks WMP word existence if active to prune shadow plays that can't form valid words.
-- - For bingo with playthrough: check if bingo exists with rack + playthrough tiles
-- - For nonplaythrough: check if any word of that length exists from rack
shadowRecord :: ShadowConfig -> MoveGen -> MoveGen
shadowRecord _cfg gen =
  let wmg = mgWMPMoveGen gen
      tilesPlayed = mgTilesPlayed gen
      numLettersOnRack = mgNumberOfLettersOnRack gen

      -- Check WMP existence if active
      skipDueToWMP = if wmpMoveGenIsActive wmg
                     then
                       -- If playthrough and full rack (bingo): check playthrough existence
                       if wmpMoveGenHasPlaythrough wmg && tilesPlayed == numLettersOnRack
                       then not (wmpMoveGenCheckPlaythroughFullRackExistence wmg)
                       -- If no playthrough and word >= 2 letters: check nonplaythrough existence
                       else if not (wmpMoveGenHasPlaythrough wmg) && tilesPlayed >= minimumWordLength
                       then not (wmpMoveGenNonplaythroughWordOfLengthExists tilesPlayed wmg)
                       else False
                     else False

  in if skipDueToWMP
     then gen  -- Skip recording, word doesn't exist
     else let
           -- Compute score from unrestricted tiles with best multipliers
           tilesPlayedScore = computeUnrestrictedTilesScore gen

           -- Bingo bonus
           bingoBonus = if tilesPlayed >= defaultRackSize
                        then mgBingoBonus gen
                        else 0

           -- Total score
           score = tilesPlayedScore +
                   (mgShadowMainwordRestrictedScore gen * mgShadowWordMultiplier gen) +
                   mgShadowPerpAdditionalScore gen + bingoBonus

           -- For now, equity = score (would add leave values for full implementation)
           equity = score

           -- Update max tiles played
           newMaxTiles = max (mgMaxTilesToPlay gen) tilesPlayed

           -- Update highest scores
           newHighScore = max (mgHighestShadowScore gen) score
           newHighEquity = max (mgHighestShadowEquity gen) equity

       in gen
          { mgHighestShadowScore = newHighScore
          , mgHighestShadowEquity = newHighEquity
          , mgMaxTilesToPlay = newMaxTiles
          }

-- | Compute score from unrestricted tiles with best multipliers
-- Assigns highest-scoring tiles to highest multiplier positions
computeUnrestrictedTilesScore :: MoveGen -> Int
computeUnrestrictedTilesScore gen =
  let descScores = VU.toList (mgDescTileScores gen)
      descMuls = mgDescEffLetterMuls gen
      -- Pair up scores with multipliers
      pairs = zip descScores descMuls
  in sum [s * m | (s, m) <- pairs]

-- | Check if play is valid for recording (at least 2 tiles, or 1 with uniquePlay)
playIsNonemptyAndNonduplicate :: Int -> Bool -> Bool
playIsNonemptyAndNonduplicate tilesPlayed uniquePlay =
  tilesPlayed > 1 || (tilesPlayed == 1 && uniquePlay)

-- | Extract sorted anchors (for testing)
extractSortedAnchors :: AnchorHeap -> [Anchor]
extractSortedAnchors = sortBy (comparing (Down . anchorHighestPossibleEquity))
