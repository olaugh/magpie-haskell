{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Move generation for Scrabble
-- Faithful port of MAGPIE's recursive_gen algorithm using GADDAG
module Magpie.MoveGen
  ( generateMoves
  , generateMovesWithScores
  , generateBestMove
  , generateExchanges
  , scoreMove
  , MoveGenConfig(..)
  , defaultMoveGenConfig
  , computeLeave
  ) where

import Magpie.Types
import Magpie.KWG
import Magpie.KLV (KLV, klvGetLeaveValueFromTiles)
import Magpie.Board
import Magpie.LetterDistribution
import Magpie.Shadow (generateAnchors, Anchor(..), ShadowConfig(..), defaultShadowConfig)

import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32)
import Data.Bits (testBit, setBit, (.&.))
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.IntMap.Strict as IM
import Control.Monad.ST (ST, runST)
import Control.Monad (when)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- | Configuration for move generation
data MoveGenConfig = MoveGenConfig
  { mgcBingoBonus :: !Int   -- ^ Bonus for using all 7 tiles
  } deriving (Show)

-- | Default configuration
defaultMoveGenConfig :: MoveGenConfig
defaultMoveGenConfig = MoveGenConfig
  { mgcBingoBonus = 50
  }

-- | Strip for tracking placed tiles (column -> letter)
-- IntMap is efficient for small maps (15 elements) with O(log n) operations
type Strip = IM.IntMap MachineLetter

-- | Empty strip
{-# INLINE emptyStrip #-}
emptyStrip :: Strip
emptyStrip = IM.empty

-- | Insert a letter into the strip
{-# INLINE stripInsert #-}
stripInsert :: Int -> MachineLetter -> Strip -> Strip
stripInsert = IM.insert

-- | Get a letter from the strip
{-# INLINE stripGet #-}
stripGet :: Int -> Strip -> MachineLetter
stripGet col strip = IM.findWithDefault (MachineLetter 0) col strip

-- | Blank machine letter constant
{-# INLINE blankML #-}
blankML :: MachineLetter
blankML = MachineLetter 0

-- | Played-through marker (letter already on board)
{-# INLINE playedThroughML #-}
playedThroughML :: MachineLetter
playedThroughML = MachineLetter 0xFF

-- | Generate all valid moves for a rack on a board
generateMoves :: KWG -> LetterDistribution -> Board -> Rack -> [Move]
generateMoves kwg ld board rack =
  sortBy (comparing (Down . moveScore)) $
    generateMovesWithScores defaultMoveGenConfig kwg ld board rack

-- | Generate moves with scoring
generateMovesWithScores :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> [Move]
generateMovesWithScores cfg kwg ld board rack =
  let rackCrossSet = computeRackCrossSet rack (ldSize ld)

      -- Generate horizontal moves: use Horizontal cross-sets (checking Vertical perps)
      boardH = computeAnchors $ computeCrossSets kwg ld Horizontal board
      hMoves = concatMap (genMovesForRow cfg kwg ld boardH rack rackCrossSet Horizontal) [0..boardDim-1]

      -- For empty board, skip vertical moves (horizontal covers all opening moves due to symmetry)
      boardIsEmpty = getTilesPlayed board == 0
      vMoves = if boardIsEmpty
               then []
               else
                 -- Generate vertical moves: transpose, use Horizontal cross-sets on transposed board
                 -- Use Horizontal direction for generation (uniquePlay=True, all plays recorded)
                 -- Then filter out single-tile plays that have perpendicular horizontal words
                 let transBoard = transpose board
                     boardV = computeAnchors $ computeCrossSets kwg ld Horizontal transBoard
                     vMovesRaw = concatMap (genMovesForRow cfg kwg ld boardV rack rackCrossSet Horizontal) [0..boardDim-1]
                     -- Transpose move coordinates and mark as Vertical
                     vMovesTransposed = map transposeMove vMovesRaw
                     -- Deduplicate vertical moves by actual tile placement position
                     vMovesDeduped = dedupeByPlacement board vMovesTransposed
                     -- Filter out single-tile vertical plays that have a perpendicular horizontal word
                     -- These were already counted by the horizontal generator
                     -- A play has a perpendicular horizontal word if there's a tile to the left or right
                     hasPerpHorizontalWord m =
                       let (pr, pc) = findSingleTilePlacement board m
                           hasLeft = pc > 0 && not (isEmpty board pr (pc - 1))
                           hasRight = pc < boardDim - 1 && not (isEmpty board pr (pc + 1))
                       in hasLeft || hasRight
                     isDuplicateSingleTile m = moveTilesUsed m == 1 && hasPerpHorizontalWord m
                 in filter (not . isDuplicateSingleTile) vMovesDeduped

      passMove = Move Pass 0 0 Horizontal [] 0 0 (Equity 0)
      exchangeMoves = generateExchanges rack
  in passMove : exchangeMoves ++ hMoves ++ vMoves

-- | Generate only the best move using shadow pruning and ST-based generation
-- This is much faster than generateMoves when you only need the top move
-- because it:
-- 1. Uses shadow pruning to skip anchors that can't beat current best
-- 2. Uses ST monad with mutable vectors for O(1) rack/strip operations
-- 3. Never builds lists of moves - only tracks the best move found
--
-- If a KLV is provided, moves are compared by equity (score + leave value).
-- Otherwise, moves are compared by score only.
--
-- bagCount: number of tiles remaining in the bag (exchanges require >= 7)
generateBestMove :: MoveGenConfig -> Maybe KLV -> KWG -> LetterDistribution -> Board -> Rack -> Int -> Move
generateBestMove cfg mKlv kwg ld board rack bagCount =
  let rackCrossSet = computeRackCrossSet rack (ldSize ld)
      distSize = ldSize ld

      -- Prepare boards for both directions (pre-compute cross-sets once)
      -- Cross-sets for Horizontal moves (checking Vertical perpendiculars)
      boardH = computeAnchors $ computeCrossSets kwg ld Horizontal board
      -- For Vertical moves: transpose board so columns become rows (cache-friendly)
      -- Then compute cross-sets as if Horizontal on the transposed board
      transBoard = transpose board
      boardVTrans = computeAnchors $ computeCrossSets kwg ld Horizontal transBoard

      -- Generate shadow anchors (sorted by highest possible equity descending)
      shadowCfg = defaultShadowConfig { shadowBingoBonus = mgcBingoBonus cfg }
      anchors = generateAnchors shadowCfg kwg ld board rack

      -- Process anchors in order, stopping when we can't beat the best
      passMove = Move Pass 0 0 Horizontal [] 0 0 (Equity 0)

      -- Compute equity for a move using leave values
      computeEquity :: Move -> Equity
      computeEquity m =
        let score = moveScore m
            tiles = moveTiles m
            leaveVal = case mKlv of
              Just klv -> klvGetLeaveValueFromTiles klv rack tiles
              Nothing  -> 0
        in Equity (fromIntegral score * 1000 + leaveVal)

      -- Update a move with computed equity
      withEquity :: Move -> Move
      withEquity m = m { moveEquity = computeEquity m }

      -- Compute initial equity for pass move
      passEquity = computeEquity passMove
      passWithEquity = passMove { moveEquity = passEquity }

      -- Generate exchange moves (only allowed when bag has >= 7 tiles)
      exchangeMoves = if bagCount >= 7
                      then map withEquity (generateExchanges rack)
                      else []

      -- Find best non-placement move (pass or exchange)
      (initialBest, initialBestEquity) =
        case exchangeMoves of
          [] -> (passWithEquity, passEquity)
          _  -> let bestExchange = maximumByEquity exchangeMoves
                    bestExchangeEquity = moveEquity bestExchange
                in if bestExchangeEquity > passEquity
                   then (bestExchange, bestExchangeEquity)
                   else (passWithEquity, passEquity)

      -- Maximum possible leave value for any rack subset (conservative upper bound)
      -- This is needed for shadow pruning - raw KLV values max around 75 points
      -- In fixed-point (1000x): 75 * 1000 = 75000
      maxLeaveValue :: Int32
      maxLeaveValue = case mKlv of
        Nothing -> 0
        Just _  -> 75000

      -- Run the entire anchor loop in a single ST computation
      -- This avoids re-allocating mutable state for each anchor
  in runST $ do
    strip <- newMStrip
    rackM <- thawRackCounts rack
    bestRef <- newSTRef (initialBest, initialBestEquity)

    let loop [] = return ()
        loop (anchor:rest) = do
          (_, Equity bestEquity) <- readSTRef bestRef
          -- Prune: if this anchor's upper bound + max leave can't beat our best equity, we're done
          let anchorMaxEquity = fromIntegral (anchorHighestPossibleScore anchor) * 1000 + maxLeaveValue
          if anchorMaxEquity <= bestEquity
            then return ()
            else do
              let dir = anchorDir anchor
                  ancRow = anchorRow anchor
                  ancCol = anchorCol anchor
                  lastAnchorCol = anchorLastAnchorCol anchor
                  -- For Vertical: swap row/col so "row" is the line of travel (column in board space)
                  -- For Horizontal: row/col stay as-is
                  (genRow, genAncCol, genLastAncCol) = case dir of
                    Horizontal -> (ancRow, ancCol, lastAnchorCol)
                    Vertical   -> (ancCol, ancRow, lastAnchorCol)
                  boardForDir = case dir of
                    Horizontal -> boardH
                    Vertical   -> boardVTrans
                  initialUniquePlay = case dir of
                    Horizontal -> True
                    Vertical   -> False

              recursiveGenBestSTDir cfg mKlv rack maxLeaveValue kwg ld boardForDir rackM rackCrossSet
                                    dir genRow genAncCol genLastAncCol
                                    (gaddagRoot kwg) genAncCol genAncCol genAncCol
                                    initialUniquePlay 0 1 0 0 distSize strip bestRef

              loop rest

    loop anchors
    (finalBest, _) <- readSTRef bestRef
    return finalBest
  where
    maximumByEquity [] = Move Pass 0 0 Horizontal [] 0 0 (Equity 0)
    maximumByEquity (x:xs) = go x xs
      where go best [] = best
            go best (y:ys) = go (if moveEquity y > moveEquity best then y else best) ys

-- | Find the actual tile placement position for a single-tile move
-- Walks along the word direction to find the empty square where the tile is placed
findSingleTilePlacement :: Board -> Move -> (Int, Int)
findSingleTilePlacement board m =
  let row = moveRow m
      col = moveCol m
      (dr, dc) = case moveDir m of
        Horizontal -> (0, 1)
        Vertical -> (1, 0)
      -- Walk along direction until we find an empty square
      go r c
        | r >= boardDim || c >= boardDim = (row, col)  -- fallback (shouldn't happen)
        | isEmpty board r c = (r, c)  -- found the empty square
        | otherwise = go (r + dr) (c + dc)
  in go row col

-- | Deduplicate moves by actual tile placement position (for single-tile plays)
-- or by (row, col, tiles) for multi-tile plays
dedupeByPlacement :: Board -> [Move] -> [Move]
dedupeByPlacement board = go []
  where
    go _ [] = []
    go seen (m:ms)
      | key `elem` seen = go seen ms
      | otherwise = m : go (key:seen) ms
      where
        -- For single-tile plays, use actual placement position
        -- For multi-tile plays, use word position
        key = if moveTilesUsed m == 1
              then let (pr, pc) = findSingleTilePlacement board m
                   in (pr, pc, moveTiles m)
              else (moveRow m, moveCol m, moveTiles m)

-- | Transpose a move's coordinates (for vertical move generation)
transposeMove :: Move -> Move
transposeMove m = m { moveRow = moveCol m, moveCol = moveRow m, moveDir = Vertical }

-- | Compute rack cross set - bitmask of letters on rack
-- If rack has a blank, include all letters (blank can represent any letter)
computeRackCrossSet :: Rack -> Int -> Word64
computeRackCrossSet rack distSize =
  let hasBlank = rackHasLetter blankML rack
      allLetters = foldr (\i acc -> setBit acc i) 0 [1 .. distSize - 1]  -- All letters (1 to distSize-1)
      naturalLetters = foldr (\i acc -> if rackHasLetter (MachineLetter (fromIntegral i)) rack
                                        then setBit acc i
                                        else acc) 0 [1 .. distSize - 1]
  in if hasBlank
     then allLetters  -- Blank can play as any letter
     else naturalLetters

-- | Generate exchange moves (unique combinations based on tile counts)
generateExchanges :: Rack -> [Move]
generateExchanges rack =
  let -- Get list of (letter, count) pairs for tiles on rack
      tileCounts = [(MachineLetter (fromIntegral i), VU.unsafeIndex (rackCounts rack) i)
                   | i <- [0 .. VU.length (rackCounts rack) - 1]
                   , VU.unsafeIndex (rackCounts rack) i > 0]
      -- Generate all possible combinations of counts (0 to n for each tile type)
      allCombos = generateCountCombinations tileCounts
      -- Remove the empty combination
      nonEmptyCombos = filter (not . null) allCombos
  in map makeExchange nonEmptyCombos
  where
    makeExchange tiles = Move Exchange 0 0 Horizontal tiles (length tiles) 0 (Equity 0)

-- | Generate all combinations of tile counts
-- For each (letter, count), we can take 0, 1, ..., count of that letter
generateCountCombinations :: [(MachineLetter, Int)] -> [[MachineLetter]]
generateCountCombinations [] = [[]]
generateCountCombinations ((ml, count):rest) =
  let restCombos = generateCountCombinations rest
  in [replicate n ml ++ combo | n <- [0..count], combo <- restCombos]

-- | Generate moves for a single row
genMovesForRow :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> Word64
               -> Direction -> Int -> [Move]
genMovesForRow cfg kwg ld board rack rackCrossSet dir row =
  let anchors = [c | c <- [0..boardDim-1], isAnchor board row c]
  in processAnchors cfg kwg ld board rack rackCrossSet dir row boardDim anchors

-- | Process anchors with lastAnchorCol tracking
processAnchors :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> Word64
               -> Direction -> Int -> Int -> [Int] -> [Move]
processAnchors _ _ _ _ _ _ _ _ _ [] = []
processAnchors cfg kwg ld board rack rackCrossSet dir row lastAnchorCol (anchorCol:rest) =
  let initialUniquePlay = case dir of
        Horizontal -> True
        Vertical -> False

      -- Start at anchor with initial state
      movesFromAnchor = recursiveGen cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                                     (gaddagRoot kwg) anchorCol  -- col = anchorCol
                                     anchorCol anchorCol  -- leftstrip = rightstrip = anchorCol
                                     initialUniquePlay 0 1 0 0  -- tilesPlayed=0
                                     emptyStrip  -- empty strip

      -- Update lastAnchorCol: if current anchor is occupied, next can start after it
      newLastAnchorCol = if not (isEmpty board row anchorCol)
                         then anchorCol + 1
                         else anchorCol

      restMoves = processAnchors cfg kwg ld board rack rackCrossSet dir row newLastAnchorCol rest
  in movesFromAnchor ++ restMoves

-- | Check if play is valid for recording (matches MAGPIE's play_is_nonempty_and_nonduplicate)
playIsNonemptyAndNonduplicate :: Int -> Bool -> Bool
playIsNonemptyAndNonduplicate tilesPlayed uniquePlay =
  (tilesPlayed > 1) || ((tilesPlayed == 1) && uniquePlay)

-- | Main GADDAG move generation (matches MAGPIE's recursive_gen)
-- Uses accumulator pattern to avoid O(n) list concatenations
-- col is the current position being examined
-- leftstrip/rightstrip track word bounds
-- tilesPlayed tracks tiles placed from rack
-- strip tracks what letter is at each position
recursiveGen :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> Word64
             -> Direction -> Int -> Int -> Int -> Word32
             -> Int  -- col (current position)
             -> Int -> Int  -- leftstrip, rightstrip
             -> Bool -> Int -> Int -> Int -> Int  -- uniquePlay, mainWordScore, wordMult, crossScore, tilesPlayed
             -> Strip
             -> [Move]
recursiveGen cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol nodeIdx
             col leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore tilesPlayed strip =
  recursiveGenAcc cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol nodeIdx
                  col leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore tilesPlayed strip []

-- | Accumulator-based version of recursiveGen
{-# INLINE recursiveGenAcc #-}
recursiveGenAcc :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> Word64
             -> Direction -> Int -> Int -> Int -> Word32
             -> Int -> Int -> Int -> Bool -> Int -> Int -> Int -> Int -> Strip
             -> [Move] -> [Move]
recursiveGenAcc cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol nodeIdx
             col leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore tilesPlayed strip !acc
  | nodeIdx == 0 = acc
  | col < 0 || col >= boardDim = acc
  | otherwise =
      let currentLetter = getLetter board row col
          crossSet = getCrossSet board row col
          -- If crossSet is 1 (only bit 0 = blank allowed), no letters can be placed
          possibleLettersHere = if crossSet == 1 then 0 else crossSet

      in if unML currentLetter /= 0
         then -- Play through occupied square
           let rawLetter = unblankLetter currentLetter
               (nextNodeIdx, accepts) = getNextNodeAndAccepts kwg nodeIdx rawLetter
               -- Mark as played-through in strip
               newStrip = stripInsert col playedThroughML strip
           in if nextNodeIdx == 0 && not accepts
              then acc
              else goOnAcc cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                        nextNodeIdx accepts col currentLetter
                        leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore tilesPlayed newStrip acc

         else -- Try placing from rack
           if not (rackIsEmpty rack) && ((possibleLettersHere .&. rackCrossSet) /= 0)
           then tryAllLettersAtNodeAcc cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                                    nodeIdx col leftstrip rightstrip uniquePlay mainWordScore wordMult
                                    crossScore possibleLettersHere tilesPlayed strip acc
           else acc

-- | Try all valid letters at node (matches MAGPIE's letter iteration loop)
-- Uses accumulator to avoid list concatenations
{-# INLINE tryAllLettersAtNodeAcc #-}
tryAllLettersAtNodeAcc :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> Word64
                    -> Direction -> Int -> Int -> Int -> Word32
                    -> Int -> Int -> Int -> Bool -> Int -> Int -> Int -> Word64 -> Int -> Strip
                    -> [Move] -> [Move]
tryAllLettersAtNodeAcc cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol nodeIdx
                    col leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore possibleLetters tilesPlayed strip !acc0 =
  go nodeIdx acc0
  where
    go !i !acc =
      let node = getNode kwg i
          tile = nodeTile node
          ml = MachineLetter (fromIntegral tile)
          numberOfMl = rackGetCount ml rack
          hasBlank = rackHasLetter blankML rack
          accepts = nodeAccepts node
          nextNodeIdx = nodeArcIndex node
          isEnd = nodeIsEnd node

          canPlace = tile /= 0 &&
                     (numberOfMl > 0 || hasBlank) &&
                     testBit possibleLetters (fromIntegral tile)

          -- Process natural tile placement
          acc1 = if canPlace && numberOfMl > 0
                 then let newRack = rackTakeLetter ml rack
                          newStrip = stripInsert col ml strip
                      in goOnAcc cfg kwg ld board newRack rackCrossSet dir row
                              anchorCol lastAnchorCol nextNodeIdx accepts
                              col ml leftstrip rightstrip uniquePlay
                              mainWordScore wordMult crossScore (tilesPlayed + 1) newStrip acc
                 else acc

          -- Process blank tile placement
          acc2 = if canPlace && hasBlank
                 then let newRack = rackTakeLetter blankML rack
                          blankedML = blankLetter ml  -- Blank representing this letter
                          newStrip = stripInsert col blankedML strip
                      in goOnAcc cfg kwg ld board newRack rackCrossSet dir row
                              anchorCol lastAnchorCol nextNodeIdx accepts
                              col blankedML leftstrip rightstrip uniquePlay
                              mainWordScore wordMult crossScore (tilesPlayed + 1) newStrip acc1
                 else acc1

      in if isEnd then acc2 else go (i + 1) acc2

-- | Continue generation after placing/traversing a letter (matches MAGPIE's go_on)
-- Uses accumulator pattern to avoid list concatenation
{-# INLINE goOnAcc #-}
goOnAcc :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> Word64
     -> Direction -> Int -> Int -> Int -> Word32 -> Bool
     -> Int -> MachineLetter  -- current_col, letter placed
     -> Int -> Int -> Bool -> Int -> Int -> Int -> Int  -- leftstrip, rightstrip, uniquePlay, scores, tilesPlayed
     -> Strip
     -> [Move] -> [Move]
goOnAcc cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol newNodeIdx accepts
     currentCol letterPlaced leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore tilesPlayed strip !acc =
  let -- Determine if this is a fresh tile (from rack) or playing through
      squareIsEmpty = isEmpty board row currentCol

      -- Get bonus square info
      sq = getSquare board row currentCol
      bonus = sqBonus sq

      -- Scoring increments
      letterMult = if squareIsEmpty then letterMultiplier bonus else 1
      thisWordMult = if squareIsEmpty then wordMultiplier bonus else 1
      newWordMult = wordMult * thisWordMult

      -- Letter score (blank scores 0)
      ml = unblankLetter letterPlaced
      tileScore = if isBlank letterPlaced then 0 else ldScore ld ml
      scoredLetter = tileScore * letterMult
      newMainScore = mainWordScore + scoredLetter

      -- Cross word scoring (only for fresh tiles with perpendicular words)
      crossWordBase = if squareIsEmpty then getCrossScore board row currentCol else 0
      newCrossScore = if crossWordBase > 0
                      then crossScore + (crossWordBase + scoredLetter) * thisWordMult
                      else crossScore

      distSize = ldSize ld

  in if currentCol <= anchorCol
     then -- At or left of anchor
       let -- Update uniquePlay for vertical direction
           newUniquePlay = case dir of
             Horizontal -> uniquePlay
             Vertical -> if squareIsEmpty && getCrossSet board row currentCol == trivialCrossSet distSize
                         then True
                         else uniquePlay

           newLeftstrip = currentCol
           noLetterDirectlyLeft = currentCol == 0 || isEmpty board row (currentCol - 1)
           -- Also check there's no tile to the right of anchor (word must be complete on both sides)
           noLetterRightOfAnchor = anchorCol == boardDim - 1 || isEmpty board row (anchorCol + 1)

           -- Record move if this is a valid word end (no letters on either side)
           acc1 = if accepts && noLetterDirectlyLeft && noLetterRightOfAnchor &&
                     playIsNonemptyAndNonduplicate tilesPlayed newUniquePlay
                  then buildMove cfg board row newLeftstrip rightstrip newMainScore newWordMult newCrossScore tilesPlayed strip : acc
                  else acc

           -- Continue left (if node continues and we haven't hit lastAnchorCol)
           acc2 = if newNodeIdx /= 0 && currentCol > 0 && (currentCol - 1) /= lastAnchorCol
                  then recursiveGenAcc cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                                        newNodeIdx (currentCol - 1) newLeftstrip rightstrip
                                        newUniquePlay newMainScore newWordMult newCrossScore tilesPlayed strip acc1
                  else acc1

           -- Cross separator to go right (if valid)
           separatorNode = getSeparatorArc kwg newNodeIdx
           acc3 = if separatorNode /= 0 && noLetterDirectlyLeft && anchorCol < boardDim - 1
                  then recursiveGenAcc cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                                         separatorNode (anchorCol + 1) newLeftstrip rightstrip
                                         newUniquePlay newMainScore newWordMult newCrossScore tilesPlayed strip acc2
                  else acc2

       in acc3

     else -- Right of anchor
       let -- Update uniquePlay for vertical direction
           newUniquePlay = case dir of
             Horizontal -> uniquePlay
             Vertical -> if squareIsEmpty && not uniquePlay && getCrossSet board row currentCol == trivialCrossSet distSize
                         then True
                         else uniquePlay

           newRightstrip = currentCol
           noLetterDirectlyRight = currentCol == boardDim - 1 || isEmpty board row (currentCol + 1)

           -- Record move if this is a valid word end
           acc1 = if accepts && noLetterDirectlyRight &&
                     playIsNonemptyAndNonduplicate tilesPlayed newUniquePlay
                  then buildMove cfg board row leftstrip newRightstrip newMainScore newWordMult newCrossScore tilesPlayed strip : acc
                  else acc

           -- Continue right
           acc2 = if newNodeIdx /= 0 && currentCol < boardDim - 1
                  then recursiveGenAcc cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                                         newNodeIdx (currentCol + 1) leftstrip newRightstrip
                                         newUniquePlay newMainScore newWordMult newCrossScore tilesPlayed strip acc1
                  else acc1

       in acc2

-- | Get separator arc (transition from prefix to suffix in GADDAG)
getSeparatorArc :: KWG -> Word32 -> Word32
getSeparatorArc kwg nodeIdx
  | nodeIdx == 0 = 0
  | otherwise = go nodeIdx
  where
    go i = let node = getNode kwg i
               tile = nodeTile node
           in if tile == 0 then nodeArcIndex node  -- tile 0 is separator
              else if nodeIsEnd node then 0
              else go (i + 1)

-- | Get next node and accepts flag for a letter
getNextNodeAndAccepts :: KWG -> Word32 -> MachineLetter -> (Word32, Bool)
getNextNodeAndAccepts kwg nodeIdx (MachineLetter letter)
  | nodeIdx == 0 = (0, False)
  | otherwise = go nodeIdx
  where
    letterW = fromIntegral letter
    go !i = let node = getNode kwg i
                tile = nodeTile node
            in if tile == letterW then (nodeArcIndex node, nodeAccepts node)
               else if nodeIsEnd node then (0, False)
               else go (i + 1)

-- | Build a move from generation state, extracting tiles from strip
buildMove :: MoveGenConfig -> Board -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Strip -> Move
buildMove cfg _board row leftstrip rightstrip mainScore wordMult crossScore tilesUsed strip =
  let finalMainScore = mainScore * wordMult
      bingoBonus = if tilesUsed >= defaultRackSize then mgcBingoBonus cfg else 0
      totalScore = finalMainScore + crossScore + bingoBonus
      -- Extract tiles from strip, skipping played-through markers
      tiles = [ ml | col <- [leftstrip..rightstrip]
                   , let ml = stripGet col strip
                   , ml /= playedThroughML  -- Skip played-through tiles
              ]
  in Move
     { moveType = TilePlacement
     , moveRow = row
     , moveCol = leftstrip
     , moveDir = Horizontal
     , moveTiles = tiles
     , moveTilesUsed = tilesUsed
     , moveScore = totalScore
     , moveEquity = Equity (fromIntegral totalScore * 1000)
     }

-- | Check if rack is empty
rackIsEmpty :: Rack -> Bool
rackIsEmpty rack = VU.all (== 0) (rackCounts rack)

-- | Compute the leave (remaining rack) after playing a move
-- Takes the original rack and the tiles played, returns remaining tiles
computeLeave :: Rack -> [MachineLetter] -> Rack
computeLeave rack tiles = foldr removeTile rack tiles
  where
    -- Remove a tile from the rack
    -- For blanked letters (high bit set), remove a blank (ML 0)
    -- For regular letters, remove the letter itself
    removeTile ml r =
      if isBlank ml
        then rackTakeLetter blankML r  -- Blanked letter: take blank from rack
        else rackTakeLetter ml r        -- Regular letter: take that letter

-- | Score a move (public API)
scoreMove :: MoveGenConfig -> LetterDistribution -> Board -> Move -> Int
scoreMove cfg ld board move =
  case moveType move of
    Pass -> 0
    Exchange -> 0
    TilePlacement -> computeMoveScore cfg ld board move

-- | Compute score for a tile placement move
computeMoveScore :: MoveGenConfig -> LetterDistribution -> Board -> Move -> Int
computeMoveScore cfg ld board move =
  let tiles = moveTiles move
      row = moveRow move
      col = moveCol move
      dir = moveDir move

      tilesUsed = length tiles
      bingoBonus = if tilesUsed >= defaultRackSize then mgcBingoBonus cfg else 0

      mainScore = computeWordScore ld board row col dir tiles
      crossScores = computeCrossScores ld board row col dir tiles

  in mainScore + crossScores + bingoBonus

-- | Compute main word score
computeWordScore :: LetterDistribution -> Board -> Int -> Int -> Direction -> [MachineLetter] -> Int
computeWordScore ld board startRow startCol dir tiles =
  let (dr, dc) = case dir of
        Horizontal -> (0, 1)
        Vertical -> (1, 0)

      go _ _ [] wordMult total = total * wordMult
      go r c (t:ts) wordMult total =
        let sq = getSquare board r c
            bonus = sqBonus sq
            letterMult = letterMultiplier bonus
            wMult = wordMultiplier bonus
            tileScore = if isBlank t then 0 else ldScore ld (unblankLetter t)
        in go (r + dr) (c + dc) ts (wordMult * wMult) (total + tileScore * letterMult)
  in go startRow startCol tiles 1 0

-- | Compute cross word scores
computeCrossScores :: LetterDistribution -> Board -> Int -> Int -> Direction -> [MachineLetter] -> Int
computeCrossScores ld board startRow startCol dir tiles =
  let (dr, dc) = case dir of
        Horizontal -> (0, 1)
        Vertical -> (1, 0)

      go _ _ [] total = total
      go r c (t:ts) total =
        let sq = getSquare board r c
            bonus = sqBonus sq
            letterMult = letterMultiplier bonus
            wMult = wordMultiplier bonus
            tileScore = if isBlank t then 0 else ldScore ld (unblankLetter t)
            crossScoreBase = getCrossScore board r c
            wordScore = if crossScoreBase > 0
                        then (crossScoreBase + tileScore * letterMult) * wMult
                        else 0
        in go (r + dr) (c + dc) ts (total + wordScore)
  in go startRow startCol tiles 0

-- ============================================================================
-- ST-based best-move-only generation (high-performance path)
-- ============================================================================

-- | Mutable strip for tracking placed tiles (15 elements, O(1) access)
type MStrip s = MVU.MVector s Word8

-- | Create a new mutable strip
{-# INLINE newMStrip #-}
newMStrip :: ST s (MStrip s)
newMStrip = MVU.replicate boardDim 0

-- | Write to mutable strip
{-# INLINE mstripWrite #-}
mstripWrite :: MStrip s -> Int -> MachineLetter -> ST s ()
mstripWrite strip col (MachineLetter ml) = MVU.unsafeWrite strip col ml

-- | Read from mutable strip
{-# INLINE mstripRead #-}
mstripRead :: MStrip s -> Int -> ST s MachineLetter
mstripRead strip col = MachineLetter <$> MVU.unsafeRead strip col

-- | Clear a position in mutable strip
{-# INLINE mstripClear #-}
mstripClear :: MStrip s -> Int -> ST s ()
mstripClear strip col = MVU.unsafeWrite strip col 0

-- | Mutable rack counts (distSize elements, O(1) access)
type MRackCounts s = MVU.MVector s Int

-- | Thaw rack counts into mutable vector
{-# INLINE thawRackCounts #-}
thawRackCounts :: Rack -> ST s (MRackCounts s)
thawRackCounts rack = VU.thaw (rackCounts rack)

-- | Take a letter from mutable rack (returns new count)
{-# INLINE mrackTake #-}
mrackTake :: MRackCounts s -> MachineLetter -> ST s Int
mrackTake rackM (MachineLetter ml) = do
  let idx = fromIntegral ml
  count <- MVU.unsafeRead rackM idx
  let newCount = count - 1
  MVU.unsafeWrite rackM idx newCount
  return newCount

-- | Return a letter to mutable rack
{-# INLINE mrackReturn #-}
mrackReturn :: MRackCounts s -> MachineLetter -> ST s ()
mrackReturn rackM (MachineLetter ml) = do
  let idx = fromIntegral ml
  MVU.unsafeModify rackM (+1) idx

-- | Check if mutable rack has a letter
{-# INLINE mrackHas #-}
mrackHas :: MRackCounts s -> MachineLetter -> ST s Bool
mrackHas rackM (MachineLetter ml) = do
  count <- MVU.unsafeRead rackM (fromIntegral ml)
  return (count > 0)

-- | Get count of a letter in mutable rack
{-# INLINE mrackGetCount #-}
mrackGetCount :: MRackCounts s -> MachineLetter -> ST s Int
mrackGetCount rackM (MachineLetter ml) = MVU.unsafeRead rackM (fromIntegral ml)

-- | Check if mutable rack is empty
{-# INLINE mrackIsEmpty #-}
mrackIsEmpty :: MRackCounts s -> Int -> ST s Bool
mrackIsEmpty rackM distSize = go 0
  where
    go !i | i >= distSize = return True
          | otherwise = do
              count <- MVU.unsafeRead rackM i
              if count > 0 then return False else go (i + 1)

-- | Reference to best move found so far
type BestMoveRef s = STRef s (Move, Equity)  -- (move, equity)

-- | Try to record a move, but only build it if score could beat current best
-- This defers move construction for moves that can't possibly win
-- maxLeaveVal is the maximum possible leave value (75000 in fixed-point for KLV)
{-# INLINE tryRecordMove #-}
tryRecordMove :: MoveGenConfig -> Maybe KLV -> Rack -> Int32 -> BestMoveRef s
              -> Direction -> Int -> Int -> Int -> Int -> Int -> Int -> Int
              -> MStrip s -> ST s ()
tryRecordMove cfg mKlv origRack maxLeaveVal bestRef dir row leftstrip rightstrip
              mainScore wordMult crossScore tilesUsed strip = do
  let finalMainScore = mainScore * wordMult
      bingoBonus = if tilesUsed >= defaultRackSize then mgcBingoBonus cfg else 0
      totalScore = finalMainScore + crossScore + bingoBonus
      -- Max possible equity for this move
      maxPossibleEquity = Equity (fromIntegral totalScore * 1000 + maxLeaveVal)
  (_, currentBest) <- readSTRef bestRef
  -- Only build the full move if it might beat current best
  when (maxPossibleEquity > currentBest) $ do
    tiles <- extractTilesST strip leftstrip rightstrip
    let leaveVal = case mKlv of
          Just klv -> klvGetLeaveValueFromTiles klv origRack tiles
          Nothing  -> 0
        equity = Equity (fromIntegral totalScore * 1000 + leaveVal)
    when (equity > currentBest) $ do
      let (outRow, outCol) = case dir of
            Horizontal -> (row, leftstrip)
            Vertical   -> (leftstrip, row)
          move = Move
            { moveType = TilePlacement
            , moveRow = outRow
            , moveCol = outCol
            , moveDir = dir
            , moveTiles = tiles
            , moveTilesUsed = tilesUsed
            , moveScore = totalScore
            , moveEquity = equity
            }
      writeSTRef bestRef (move, equity)

-- | Extract tiles from mutable strip
{-# INLINE extractTilesST #-}
extractTilesST :: MStrip s -> Int -> Int -> ST s [MachineLetter]
extractTilesST strip leftstrip rightstrip = go leftstrip []
  where
    go !col !acc
      | col > rightstrip = return (reverse acc)
      | otherwise = do
          ml <- mstripRead strip col
          if unML ml == 0xFF  -- played-through marker
            then go (col + 1) acc
            else go (col + 1) (ml : acc)

-- | ST-based recursive generation that only tracks the best move
-- For Horizontal: uses board directly
-- For Vertical: uses transposed board (columns become rows for cache locality)
-- The dir parameter is used for output coordinate transformation in tryRecordMove
recursiveGenBestSTDir :: MoveGenConfig -> Maybe KLV -> Rack -> Int32 -> KWG -> LetterDistribution -> Board
                      -> MRackCounts s -> Word64 -> Direction -> Int -> Int -> Int -> Word32
                      -> Int -> Int -> Int -> Bool -> Int -> Int -> Int -> Int
                      -> Int -> MStrip s -> BestMoveRef s -> ST s ()
recursiveGenBestSTDir cfg mKlv origRack maxLeaveVal kwg ld board rackM rackCrossSet
                      dir row anchorCol lastAnchorCol initialNodeIdx
                      initialCol initialLeftstrip initialRightstrip initialUniquePlay
                      initialMainScore initialWordMult initialCrossScore initialTilesPlayed
                      distSize strip bestRef =
  recGen initialNodeIdx initialCol initialLeftstrip initialRightstrip
         initialUniquePlay initialMainScore initialWordMult initialCrossScore initialTilesPlayed
  where
    -- Captured state: cfg, mKlv, origRack, kwg, ld, board, rackM, rackCrossSet,
    --                 dir, row, anchorCol, lastAnchorCol, distSize, strip, bestRef

    recGen !nodeIdx !col !leftstrip !rightstrip !uniquePlay !mainWordScore !wordMult !crossScore !tilesPlayed
      | nodeIdx == 0 = return ()
      | col < 0 || col >= boardDim = return ()
      | otherwise = do
          let currentLetter = getLetter board row col
              crossSet = getCrossSet board row col
              possibleLettersHere = if crossSet == 1 then 0 else crossSet

          if unML currentLetter /= 0
            then do
              let rawLetter = unblankLetter currentLetter
                  (nextNodeIdx, accepts) = getNextNodeAndAccepts kwg nodeIdx rawLetter
              when (nextNodeIdx /= 0 || accepts) $ do
                mstripWrite strip col playedThroughML
                goOn nextNodeIdx accepts col currentLetter leftstrip rightstrip
                     uniquePlay mainWordScore wordMult crossScore tilesPlayed
                mstripClear strip col
            else do
              isEmpty' <- mrackIsEmpty rackM distSize
              when (not isEmpty' && ((possibleLettersHere .&. rackCrossSet) /= 0)) $
                tryAllLetters nodeIdx col leftstrip rightstrip uniquePlay mainWordScore wordMult
                              crossScore possibleLettersHere tilesPlayed

    tryAllLetters !nodeIdx !col !leftstrip !rightstrip !uniquePlay !mainWordScore !wordMult
                  !crossScore !possibleLetters !tilesPlayed = go nodeIdx
      where
        go !i = do
          let node = getNode kwg i
              tile = nodeTile node
              ml = MachineLetter (fromIntegral tile)
              accepts = nodeAccepts node
              nextNodeIdx = nodeArcIndex node
              isEnd = nodeIsEnd node

          when (tile /= 0 && testBit possibleLetters (fromIntegral tile)) $ do
            numberOfMl <- mrackGetCount rackM ml
            when (numberOfMl > 0) $ do
              _ <- mrackTake rackM ml
              mstripWrite strip col ml
              goOn nextNodeIdx accepts col ml leftstrip rightstrip uniquePlay
                   mainWordScore wordMult crossScore (tilesPlayed + 1)
              mstripClear strip col
              mrackReturn rackM ml

            hasBlank <- mrackHas rackM blankML
            when hasBlank $ do
              let blankedML = blankLetter ml
              _ <- mrackTake rackM blankML
              mstripWrite strip col blankedML
              goOn nextNodeIdx accepts col blankedML leftstrip rightstrip uniquePlay
                   mainWordScore wordMult crossScore (tilesPlayed + 1)
              mstripClear strip col
              mrackReturn rackM blankML

          when (not isEnd) $ go (i + 1)

    goOn !nodeIdx !accepts !currentCol !letterPlaced !leftstrip !rightstrip
         !uniquePlay !mainWordScore !wordMult !crossScore !tilesPlayed = do
      let squareIsEmpty = isEmpty board row currentCol
          sq = getSquare board row currentCol
          bonus = sqBonus sq
          letterMult = if squareIsEmpty then letterMultiplier bonus else 1
          thisWordMult = if squareIsEmpty then wordMultiplier bonus else 1
          newWordMult = wordMult * thisWordMult
          ml = unblankLetter letterPlaced
          tileScore = if isBlank letterPlaced then 0 else ldScore ld ml
          scoredLetter = tileScore * letterMult
          newMainScore = mainWordScore + scoredLetter
          crossWordBase = if squareIsEmpty then getCrossScore board row currentCol else 0
          newCrossScore = if crossWordBase > 0
                          then crossScore + (crossWordBase + scoredLetter) * thisWordMult
                          else crossScore

      if currentCol <= anchorCol
        then do
          let newUniquePlay = case dir of
                Horizontal -> uniquePlay
                Vertical -> if squareIsEmpty && getCrossSet board row currentCol == trivialCrossSet distSize
                            then True
                            else uniquePlay
              newLeftstrip = currentCol
              noLetterDirectlyLeft = currentCol == 0 || isEmpty board row (currentCol - 1)
              noLetterRightOfAnchor = anchorCol == boardDim - 1 || isEmpty board row (anchorCol + 1)

          when (accepts && noLetterDirectlyLeft && noLetterRightOfAnchor &&
                playIsNonemptyAndNonduplicate tilesPlayed newUniquePlay) $
            tryRecordMove cfg mKlv origRack maxLeaveVal bestRef dir row newLeftstrip rightstrip
                          newMainScore newWordMult newCrossScore tilesPlayed strip

          when (nodeIdx /= 0 && currentCol > 0 && (currentCol - 1) /= lastAnchorCol) $
            recGen nodeIdx (currentCol - 1) newLeftstrip rightstrip
                   newUniquePlay newMainScore newWordMult newCrossScore tilesPlayed

          let separatorNode = getSeparatorArc kwg nodeIdx
          when (separatorNode /= 0 && noLetterDirectlyLeft && anchorCol < boardDim - 1) $
            recGen separatorNode (anchorCol + 1) newLeftstrip rightstrip
                   newUniquePlay newMainScore newWordMult newCrossScore tilesPlayed
        else do
          let newRightstrip = currentCol
              noLetterDirectlyRight = currentCol == boardDim - 1 || isEmpty board row (currentCol + 1)
              newUniquePlay = case dir of
                Horizontal -> uniquePlay
                Vertical -> if squareIsEmpty && not uniquePlay && getCrossSet board row currentCol == trivialCrossSet distSize
                            then True
                            else uniquePlay

          when (accepts && noLetterDirectlyRight &&
                playIsNonemptyAndNonduplicate tilesPlayed newUniquePlay) $
            tryRecordMove cfg mKlv origRack maxLeaveVal bestRef dir row leftstrip newRightstrip
                          newMainScore newWordMult newCrossScore tilesPlayed strip

          when (nodeIdx /= 0 && currentCol < boardDim - 1) $
            recGen nodeIdx (currentCol + 1) leftstrip newRightstrip
                   newUniquePlay newMainScore newWordMult newCrossScore tilesPlayed


