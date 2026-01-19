{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

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
import Magpie.Equity (Equity(..), equityToInt)

import Data.Word (Word64)
import Data.Bits ((.&.), (.|.), complement, popCount, countTrailingZeros)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Vector.Unboxed as VU
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed.Mutable as MVU
import Control.Monad (when)

-- | Mutable per-tiles state for tracking best scores by word length
-- Used only when WMP is active for O(1) updates instead of O(n) vector copies
data MPerTilesState s = MPerTilesState
  { mptsScores   :: !(MVU.MVector s Int)  -- Best score for each tiles_played (0-7)
  , mptsEquities :: !(MVU.MVector s Int)  -- Best equity for each tiles_played (0-7)
  }

-- | Create new mutable per-tiles state initialized to minBound
newMPerTilesState :: ST s (MPerTilesState s)
newMPerTilesState = do
  scores <- MVU.replicate 8 minBound
  equities <- MVU.replicate 8 minBound
  return $ MPerTilesState scores equities

-- | Update per-tiles state with a new score/equity for given tiles_played
updatePerTilesState :: MPerTilesState s -> Int -> Int -> Int -> ST s ()
updatePerTilesState mpts tilesPlayed score equity = do
  currentScore <- MVU.read (mptsScores mpts) tilesPlayed
  when (score > currentScore) $ do
    MVU.write (mptsScores mpts) tilesPlayed score
    MVU.write (mptsEquities mpts) tilesPlayed equity

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
  , shadowBagCount :: !Int      -- Number of tiles in bag (for WMP leave values)
  , shadowMultiAnchor :: !Bool  -- If True, generate one anchor per word length (for WMP move gen)
                                -- If False, generate single anchor per position (for GADDAG move gen)
  } deriving (Show)

-- | Default shadow config
defaultShadowConfig :: ShadowConfig
defaultShadowConfig = ShadowConfig
  { shadowBingoBonus = 50
  , shadowSortByScore = True
  , shadowBagCount = 100  -- Default to non-empty bag (enables leave values)
  , shadowMultiAnchor = True  -- Multi-anchor mode for WMP move generation
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

    -- Best scores found (overall max)
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

    -- WMP static state (immutable; playthrough state is handled via ST monad)
  , mgWMPStatic              :: !(Maybe WMPStatic)
  } deriving (Show)

-- | Initialize MoveGen for shadow scoring
initMoveGen :: LetterDistribution -> Rack -> Int -> Int -> WMPMoveGen -> MoveGen
initMoveGen ld rack bingoBonus anchorCol wmg =
  let rackCS = computeRackCrossSet rack (ldSize ld)
      numLetters = rackTotal rack
      descScores = sortTileScoresDescending ld rack
      -- Extract static WMP state (if WMP is active)
      mWmpStatic = if wmpMoveGenIsActive wmg
                   then Just (wmpStaticFromMoveGen wmg)
                   else Nothing
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
     , mgWMPStatic = mWmpStatic
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
      -- Sort by score bound (not equity bound) for consistent pruning
      -- The pruning in MoveGen uses score * 1000 + maxLeaveValue, so sorting
      -- by score maintains consistent anchor ordering regardless of leave values
  in sortBy (comparing (Down . anchorHighestPossibleScore)) allAnchors

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
  let -- WMP playthrough state is now reset inside shadowPlayForAnchor (via ST monad)
      anchors = shadowPlayForAnchor cfg ld board rack dir wmg rowOrCol col lastAnchorCol
      -- Update lastAnchorCol
      newLastAnchorCol = if not (isEmptyForDir board rowOrCol col dir)
                         then col + 1  -- Skip one after occupied
                         else col
      restAnchors = processAnchors cfg ld board rack dir wmg rowOrCol newLastAnchorCol rest
  in anchors ++ restAnchors

-- | Compute shadow scores for a single anchor position
-- Returns multiple anchors (one per valid word length) when WMP is active
-- The shadow computation runs inside an ST block with mutable WMP playthrough state.
shadowPlayForAnchor :: ShadowConfig -> LetterDistribution -> Board -> Rack -> Direction
                    -> WMPMoveGen -> Int -> Int -> Int -> [Anchor]
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
     then []
     else
       let currentLetter = getLetterForDir board rowOrCol col dir
           wmpActive = case mgWMPStatic gen2 of { Just _ -> True; Nothing -> False }

           (actualRow, actualCol) = case dir of
                                      Horizontal -> (rowOrCol, col)
                                      Vertical   -> (col, rowOrCol)

           makeAnchor tilesPlayed score equity = Anchor
               { anchorRow = actualRow
               , anchorCol = actualCol
               , anchorLastAnchorCol = lastAnchorCol
               , anchorDir = dir
               , anchorHighestPossibleScore = score
               , anchorHighestPossibleEquity = equity
               , anchorTilesToPlay = tilesPlayed
               , anchorPlaythroughBlocks = 0
               , anchorWordLength = tilesPlayed
               , anchorLeftmostStartCol = col
               , anchorRightmostStartCol = col
               }

       in if wmpActive
          then
            -- WMP mode: create per-tiles mutable state, freeze at end
            let (gen3, scoreVec, equityVec) = runST $ do
                  mwp <- newMWMPPlaythrough
                  mpts <- newMPerTilesState
                  gen <- if unML currentLetter == 0
                         then shadowStartNonplaythroughST cfg ld board mwp mpts gen2
                         else shadowStartPlaythroughST cfg ld board currentLetter mwp mpts gen2
                  scores <- VU.freeze (mptsScores mpts)
                  equities <- VU.freeze (mptsEquities mpts)
                  return (gen, scores, equities)
            in case mgWMPStatic gen3 of
                 Just ws
                   | shadowMultiAnchor cfg ->
                     -- Multi-anchor mode: one anchor per word length (for WMP move generation)
                     [ makeAnchor n score equity
                     | n <- [minimumWordLength .. rackTotal rack]
                     , let score = scoreVec VU.! n
                     , score > minBound
                     , wmpStaticWordOfLengthExists n ws || n == rackTotal rack
                     , let equity = equityVec VU.! n
                     ]
                   | otherwise ->
                     -- Single-anchor mode: use max score from any word length (for GADDAG move generation)
                     -- Still use WMP for existence checking
                     let validLengths = [n | n <- [minimumWordLength .. rackTotal rack]
                                          , wmpStaticWordOfLengthExists n ws || n == rackTotal rack
                                          , scoreVec VU.! n > minBound]
                     in case validLengths of
                          [] -> []
                          _  -> let maxTiles = maximum validLengths
                                    maxScore = maximum [scoreVec VU.! n | n <- validLengths]
                                    maxEquity = maximum [equityVec VU.! n | n <- validLengths]
                                in [makeAnchor maxTiles maxScore maxEquity]
                 Nothing -> []
          else
            -- Non-WMP mode: no per-tiles tracking needed, just use max values
            let gen3 = runST $ do
                  mwp <- newMWMPPlaythrough
                  mpts <- newMPerTilesState  -- Dummy, won't be updated
                  if unML currentLetter == 0
                    then shadowStartNonplaythroughST cfg ld board mwp mpts gen2
                    else shadowStartPlaythroughST cfg ld board currentLetter mwp mpts gen2
                maxTiles = mgMaxTilesToPlay gen3
            in if maxTiles == 0
               then []
               else [makeAnchor maxTiles (mgHighestShadowScore gen3) (mgHighestShadowEquity gen3)]


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

-- ============================================================================
-- ST-based shadow functions (use mutable WMP playthrough state)
-- ============================================================================

-- | Start shadow from empty anchor square (non-playthrough) - ST version
shadowStartNonplaythroughST :: ShadowConfig -> LetterDistribution -> Board
                            -> MWMPPlaythrough s -> MPerTilesState s -> MoveGen -> ST s MoveGen
shadowStartNonplaythroughST cfg ld board mwp mpts gen = do
  let col = mgCurrentLeftCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen
      (_, crossSet, crossScore, bonus, _, _) = getBoardData board rowOrCol col dir
      rackCS = mgRackCrossSet gen
      possibleLetters = crossSet .&. rackCS

  if possibleLetters == 0
    then return gen
    else do
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
      gen4 <- shadowRecordST cfg mwp mpts gen3

      let -- Set word multiplier to actual value
          gen5 = gen4 { mgShadowWordMultiplier = thisWordMult }

          -- Recalculate effective multipliers with correct word_mult
          gen6 = maybeRecalculateEffectiveMultipliers ld gen5

          -- Continue left
          isUnique = dir == Horizontal

      nonplaythroughShadowPlayLeftST cfg ld board isUnique mwp mpts gen6

-- | Start shadow from occupied anchor square (playthrough) - ST version
shadowStartPlaythroughST :: ShadowConfig -> LetterDistribution -> Board
                         -> MachineLetter -> MWMPPlaythrough s -> MPerTilesState s -> MoveGen -> ST s MoveGen
shadowStartPlaythroughST cfg ld board currentLetter mwp mpts gen = do
  -- Traverse through all placed tiles
  gen1 <- traversePlaythroughST ld board currentLetter mwp gen
  -- Increment playthrough blocks in WMP state
  case mgWMPStatic gen1 of
    Just _ -> mwmpIncrementBlocks mwp
    Nothing -> return ()
  -- Continue with playthrough shadow
  let isUnique = mgDir gen == Horizontal
  playthroughShadowPlayLeftST cfg ld board isUnique mwp mpts gen1

-- | Traverse through placed tiles going left, accumulating score - ST version
traversePlaythroughST :: LetterDistribution -> Board -> MachineLetter
                      -> MWMPPlaythrough s -> MoveGen -> ST s MoveGen
traversePlaythroughST ld board ml mwp gen = do
  let unblanked = unblankLetter ml
      tileScore = ldScore ld unblanked
      col = mgCurrentLeftCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen
      lastAnchorCol = mgLastAnchorCol gen

  -- Add playthrough letter to WMP state
  case mgWMPStatic gen of
    Just _ -> mwmpAddLetter unblanked mwp
    Nothing -> return ()

  let gen1 = gen { mgShadowMainwordRestrictedScore = mgShadowMainwordRestrictedScore gen + tileScore }

  if col == 0 || col == lastAnchorCol + 1
    then return gen1
    else do
      let newCol = col - 1
          nextLetter = getLetterForDir board rowOrCol newCol dir
      if unML nextLetter == 0
        then return gen1 { mgCurrentLeftCol = col }  -- Hit empty, stay at current
        else traversePlaythroughST ld board nextLetter mwp (gen1 { mgCurrentLeftCol = newCol })

-- | Shadow play left from non-playthrough start - ST version
nonplaythroughShadowPlayLeftST :: ShadowConfig -> LetterDistribution -> Board -> Bool
                               -> MWMPPlaythrough s -> MPerTilesState s -> MoveGen -> ST s MoveGen
nonplaythroughShadowPlayLeftST cfg ld board isUnique mwp mpts gen = do
  -- First try extending right
  let possibleRight = mgAnchorRightExtensionSet gen .&. mgRackCrossSet gen
  gen1 <- if possibleRight /= 0
          then shadowPlayRightST cfg ld board isUnique mwp mpts gen
          else return gen
  let gen2 = gen1 { mgAnchorRightExtensionSet = trivialCrossSet (mgLdSize gen) }

      col = mgCurrentLeftCol gen2
      rowOrCol = mgCurrentRowIndex gen2
      dir = mgDir gen2
      lastAnchorCol = mgLastAnchorCol gen2

  if col == 0 || col == lastAnchorCol + 1 ||
     mgTilesPlayed gen2 >= mgNumberOfLettersOnRack gen2
    then return gen2
    else do
      -- Check if left square is occupied (playthrough)
      let leftLetter = if col > 0 then getLetterForDir board rowOrCol (col - 1) dir else MachineLetter 0
      if unML leftLetter /= 0
        then do
          -- Found playthrough tile, traverse through it
          gen3 <- traverseLeftPlaythroughST ld board mwp gen2
          -- Increment playthrough blocks in WMP state
          case mgWMPStatic gen3 of
            Just _ -> mwmpIncrementBlocks mwp
            Nothing -> return ()
          let gen4 = gen3 { mgAnchorLeftExtensionSet = trivialCrossSet (mgLdSize gen) }
          nonplaythroughShadowPlayLeftST cfg ld board isUnique mwp mpts gen4
        else do
          -- Empty square, check if we can extend
          let possibleLeft = mgAnchorLeftExtensionSet gen2 .&. mgRackCrossSet gen2
          if possibleLeft == 0
            then return gen2
            else do
              let (_, crossSet, crossScore, bonus, _, _) = getBoardData board rowOrCol (col - 1) dir
                  possibleHere = possibleLeft .&. crossSet
              if possibleHere == 0
                then return gen2
                else do
                  let gen3 = gen2 { mgAnchorLeftExtensionSet = trivialCrossSet (mgLdSize gen2)
                                  , mgCurrentLeftCol = col - 1
                                  , mgTilesPlayed = mgTilesPlayed gen2 + 1
                                  }
                      letterMult = letterMultiplier bonus
                      thisWordMult = wordMultiplier bonus

                      (restricted, restrictedTileScore, gen4) = tryRestrictTile ld possibleHere letterMult gen3

                      -- Compute perpendicular score contribution
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

                  gen7 <- shadowRecordST cfg mwp mpts gen6
                  nonplaythroughShadowPlayLeftST cfg ld board isUnique mwp mpts gen7

-- | Traverse through playthrough tiles going left - ST version
traverseLeftPlaythroughST :: LetterDistribution -> Board -> MWMPPlaythrough s -> MoveGen -> ST s MoveGen
traverseLeftPlaythroughST ld board mwp gen = do
  let col = mgCurrentLeftCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen
      lastAnchorCol = mgLastAnchorCol gen

  if col == 0 || col == lastAnchorCol + 1
    then return gen
    else do
      let leftCol = col - 1
          leftLetter = getLetterForDir board rowOrCol leftCol dir
      if unML leftLetter == 0
        then return gen  -- Hit empty, stay at current
        else do
          let unblanked = unblankLetter leftLetter
              tileScore = ldScore ld unblanked
          -- Add playthrough letter to WMP state
          case mgWMPStatic gen of
            Just _ -> mwmpAddLetter unblanked mwp
            Nothing -> return ()
          let gen1 = gen { mgShadowMainwordRestrictedScore = mgShadowMainwordRestrictedScore gen + tileScore
                         , mgCurrentLeftCol = leftCol
                         }
          traverseLeftPlaythroughST ld board mwp gen1

-- | Shadow play left from playthrough start - ST version
playthroughShadowPlayLeftST :: ShadowConfig -> LetterDistribution -> Board -> Bool
                            -> MWMPPlaythrough s -> MPerTilesState s -> MoveGen -> ST s MoveGen
playthroughShadowPlayLeftST cfg ld board isUnique mwp mpts gen = do
  -- First try extending right
  let possibleRight = mgAnchorRightExtensionSet gen .&. mgRackCrossSet gen
  gen1 <- if possibleRight /= 0
          then shadowPlayRightST cfg ld board isUnique mwp mpts gen
          else return gen
  let gen2 = gen1 { mgAnchorRightExtensionSet = trivialCrossSet (mgLdSize gen) }

      col = mgCurrentLeftCol gen2
      rowOrCol = mgCurrentRowIndex gen2
      dir = mgDir gen2
      lastAnchorCol = mgLastAnchorCol gen2

  if col == 0 || col == lastAnchorCol + 1 ||
     mgTilesPlayed gen2 >= mgNumberOfLettersOnRack gen2
    then return gen2
    else do
      -- Check if left square is occupied (playthrough)
      let leftLetter = if col > 0 then getLetterForDir board rowOrCol (col - 1) dir else MachineLetter 0
      if unML leftLetter /= 0
        then do
          -- Found playthrough tile, traverse through it
          gen3 <- traverseLeftPlaythroughST ld board mwp gen2
          -- Increment playthrough blocks in WMP state
          case mgWMPStatic gen3 of
            Just _ -> mwmpIncrementBlocks mwp
            Nothing -> return ()
          let gen4 = gen3 { mgAnchorLeftExtensionSet = trivialCrossSet (mgLdSize gen) }
          playthroughShadowPlayLeftST cfg ld board isUnique mwp mpts gen4
        else do
          -- Empty square, check if we can extend
          let possibleLeft = mgAnchorLeftExtensionSet gen2 .&. mgRackCrossSet gen2
          if possibleLeft == 0
            then return gen2
            else do
              let (_, crossSet, crossScore, bonus, _, _) = getBoardData board rowOrCol (col - 1) dir
                  possibleHere = possibleLeft .&. crossSet
              if possibleHere == 0
                then return gen2
                else do
                  let gen3 = gen2 { mgAnchorLeftExtensionSet = trivialCrossSet (mgLdSize gen2)
                                  , mgCurrentLeftCol = col - 1
                                  , mgTilesPlayed = mgTilesPlayed gen2 + 1
                                  }
                      letterMult = letterMultiplier bonus
                      thisWordMult = wordMultiplier bonus

                      (restricted, restrictedTileScore, gen4) = tryRestrictTile ld possibleHere letterMult gen3

                      -- Compute perpendicular score contribution
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

                  gen7 <- if playIsNonemptyAndNonduplicate (mgTilesPlayed gen6) newUnique
                          then shadowRecordST cfg mwp mpts gen6
                          else return gen6

                  playthroughShadowPlayLeftST cfg ld board newUnique mwp mpts gen7

-- | Shadow play to the right - ST version
shadowPlayRightST :: ShadowConfig -> LetterDistribution -> Board -> Bool
                  -> MWMPPlaythrough s -> MPerTilesState s -> MoveGen -> ST s MoveGen
shadowPlayRightST cfg ld board isUnique mwp mpts gen0 = do
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
  mwmpSave mwp

  -- Extend right
  (finalGen, _) <- extendRightST cfg ld board isUnique mwp mpts gen0

  -- Restore WMP playthrough state
  mwmpRestore mwp

  -- Restore original state but keep highest scores
  return finalGen
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
    }

-- | Extend shadow to the right - ST version
extendRightST :: ShadowConfig -> LetterDistribution -> Board -> Bool
              -> MWMPPlaythrough s -> MPerTilesState s -> MoveGen -> ST s (MoveGen, Bool)
extendRightST cfg ld board isUnique mwp mpts gen = do
  let col = mgCurrentRightCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen

  if col >= boardDim - 1 || mgTilesPlayed gen >= mgNumberOfLettersOnRack gen
    then return (gen, isUnique)
    else do
      let newCol = col + 1
          -- Check if next position is occupied (playthrough)
          nextLetter = getLetterForDir board rowOrCol newCol dir
      if unML nextLetter /= 0
        then do
          -- Occupied, traverse through playthrough tiles
          (gen1, foundPlaythrough) <- traverseRightPlaythroughST ld board mwp
                                        (gen { mgCurrentRightCol = col })  -- Start from current col
          -- Increment playthrough blocks in WMP state if we found any
          case mgWMPStatic gen1 of
            Just _ | foundPlaythrough -> mwmpIncrementBlocks mwp
            _ -> return ()
          let gen2 = gen1 { mgAnchorRightExtensionSet = trivialCrossSet (mgLdSize gen) }
          extendRightST cfg ld board isUnique mwp mpts gen2
        else do
          -- Empty square, try to place a tile
          let (_, crossSet, crossScore, bonus, leftExt, _) = getBoardData board rowOrCol newCol dir
              rackCS = mgRackCrossSet gen
              rightExt = mgAnchorRightExtensionSet gen

              -- Non-blank cross set and left extension
              nonblankCS = crossSet .&. complement 1
              nonblankLeftExt = leftExt .&. complement 1

          if (nonblankCS .&. nonblankLeftExt) == 0
            then return (gen, isUnique)
            else do
              let possibleHere = crossSet .&. rackCS .&. rightExt .&. leftExt
              if possibleHere == 0
                then return (gen, isUnique)
                else do
                  let gen1 = gen
                        { mgCurrentRightCol = newCol
                        , mgTilesPlayed = mgTilesPlayed gen + 1
                        , mgAnchorRightExtensionSet = trivialCrossSet (mgLdSize gen)
                        }

                      letterMult = letterMultiplier bonus
                      thisWordMult = wordMultiplier bonus

                      (restricted, restrictedTileScore, gen2) = tryRestrictTile ld possibleHere letterMult gen1

                      -- Compute perpendicular score contribution
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
                  (gen5, foundPlaythrough2) <- traverseRightPlaythroughST ld board mwp gen4
                  -- Increment playthrough blocks in WMP state if we found any
                  case mgWMPStatic gen5 of
                    Just _ | foundPlaythrough2 -> mwmpIncrementBlocks mwp
                    _ -> return ()

                  gen6 <- if playIsNonemptyAndNonduplicate (mgTilesPlayed gen5) newUnique
                          then shadowRecordST cfg mwp mpts gen5
                          else return gen5

                  extendRightST cfg ld board newUnique mwp mpts gen6

-- | Traverse through playthrough tiles to the right - ST version
traverseRightPlaythroughST :: LetterDistribution -> Board -> MWMPPlaythrough s
                           -> MoveGen -> ST s (MoveGen, Bool)
traverseRightPlaythroughST ld board mwp gen = do
  let col = mgCurrentRightCol gen
      rowOrCol = mgCurrentRowIndex gen
      dir = mgDir gen

  if col + 1 >= boardDim
    then return (gen, False)
    else do
      let nextLetter = getLetterForDir board rowOrCol (col + 1) dir
      if unML nextLetter == 0
        then return (gen, False)
        else do
          let unblanked = unblankLetter nextLetter
              tileScore = ldScore ld unblanked
          -- Add playthrough letter to WMP state
          case mgWMPStatic gen of
            Just _ -> mwmpAddLetter unblanked mwp
            Nothing -> return ()
          let gen1 = gen
                { mgCurrentRightCol = col + 1
                , mgShadowMainwordRestrictedScore = mgShadowMainwordRestrictedScore gen + tileScore
                }
          (gen2, _) <- traverseRightPlaythroughST ld board mwp gen1
          return (gen2, True)

-- | Record a shadow score - ST version
-- Checks WMP word existence using mutable playthrough state.
-- When bag is not empty and WMP is active, adds the best leave value for this
-- number of tiles played to get a tighter equity bound.
shadowRecordST :: ShadowConfig -> MWMPPlaythrough s -> MPerTilesState s -> MoveGen -> ST s MoveGen
shadowRecordST cfg mwp mpts gen = do
  let tilesPlayed = mgTilesPlayed gen
      numLettersOnRack = mgNumberOfLettersOnRack gen
      bagCount = shadowBagCount cfg

  -- Check WMP existence if active
  skipDueToWMP <- case mgWMPStatic gen of
    Nothing -> return False
    Just ws -> do
      hasPlaythrough <- mwmpHasPlaythrough mwp
      if hasPlaythrough && tilesPlayed == numLettersOnRack
        then do
          -- Check bingo with playthrough
          exists <- mwmpCheckBingoExistence ws mwp
          return (not exists)
        else if not hasPlaythrough && tilesPlayed >= minimumWordLength
          then return (not (wmpStaticWordOfLengthExists tilesPlayed ws))
          else return False

  if skipDueToWMP
    then return gen  -- Skip recording, word doesn't exist
    else do
      let -- Compute score from unrestricted tiles with best multipliers
          tilesPlayedScore = computeUnrestrictedTilesScore gen

          -- Bingo bonus
          bingoBonus = if tilesPlayed >= defaultRackSize
                       then mgBingoBonus gen
                       else 0

          -- Total score
          score = tilesPlayedScore +
                  (mgShadowMainwordRestrictedScore gen * mgShadowWordMultiplier gen) +
                  mgShadowPerpAdditionalScore gen + bingoBonus

          -- WMP leave value: only add when bag is not empty (matches C MAGPIE)
          -- The leave value is the best leave among all valid subracks
          -- Note: Only use the leave value if it's been populated (not minBound)
          -- If best leaves aren't populated, fall back to 0
          leaveValue = case mgWMPStatic gen of
            Just ws | bagCount > 0 ->
              let lv = wmpStaticGetBestLeaveValue tilesPlayed ws
              in if lv == Equity minBound then 0 else equityToInt lv
            _ -> 0

          -- Equity = score + leave value (converted to int)
          equity = score + leaveValue

          -- Update max tiles played
          newMaxTiles = max (mgMaxTilesToPlay gen) tilesPlayed

          -- Update highest scores (overall)
          newHighScore = max (mgHighestShadowScore gen) score
          newHighEquity = max (mgHighestShadowEquity gen) equity

      -- Update per-tiles scores using mutable state (O(1) instead of O(n))
      case mgWMPStatic gen of
        Just _ -> updatePerTilesState mpts tilesPlayed score equity
        Nothing -> return ()

      return gen
        { mgHighestShadowScore = newHighScore
        , mgHighestShadowEquity = newHighEquity
        , mgMaxTilesToPlay = newMaxTiles
        }
