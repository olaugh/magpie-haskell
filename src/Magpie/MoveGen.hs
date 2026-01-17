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
import Magpie.KLV (KLV, klvGetLeaveValue, LeaveValue)
import Magpie.LeaveMap (LeaveMap, leaveMapCreate, leaveMapGetValue, leaveMapEmpty)
import Magpie.Board
import Magpie.LetterDistribution
import Magpie.Shadow (generateAnchors, Anchor(..), ShadowConfig(..), defaultShadowConfig)

import Data.Word (Word32, Word64)
import Data.Bits (testBit, setBit, (.&.))
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntMap.Strict as IM

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
type Strip = IM.IntMap MachineLetter

-- | Blank machine letter constant
blankML :: MachineLetter
blankML = MachineLetter 0

-- | Played-through marker (letter already on board)
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

-- | Generate only the best move using shadow pruning
-- This is much faster than generateMoves when you only need the top move
-- because it stops exploring anchors once remaining anchors can't beat the best found
--
-- If a KLV is provided, moves are compared by equity (score + leave value).
-- Otherwise, moves are compared by score only.
--
-- bagCount: number of tiles remaining in the bag (exchanges require >= 7)
generateBestMove :: MoveGenConfig -> Maybe KLV -> KWG -> LetterDistribution -> Board -> Rack -> Int -> Move
generateBestMove cfg mKlv kwg ld board rack bagCount =
  let rackCrossSet = computeRackCrossSet rack (ldSize ld)

      -- Prepare boards for both directions (pre-compute once)
      boardH = computeAnchors $ computeCrossSets kwg ld Horizontal board
      transBoard = transpose board
      boardVTrans = computeAnchors $ computeCrossSets kwg ld Horizontal transBoard

      -- Create LeaveMap for O(1) leave value lookups (if KLV provided)
      leaveMap = case mKlv of
        Just klv -> leaveMapCreate klv rack
        Nothing  -> leaveMapEmpty (ldSize ld)

      -- Generate shadow anchors (sorted by highest possible equity descending)
      shadowCfg = defaultShadowConfig { shadowBingoBonus = mgcBingoBonus cfg }
      anchors = generateAnchors shadowCfg kwg ld board rack

      -- Process anchors in order, stopping when we can't beat the best
      passMove = Move Pass 0 0 Horizontal [] 0 0 (Equity 0)

      -- Compute equity for a move using leave values
      -- Returns Equity (fixed-point with 1000x resolution, same as LeaveValue)
      computeEquity :: Move -> Equity
      computeEquity m =
        let score = moveScore m
            tiles = moveTiles m
            leave = computeLeave rack tiles
            leaveVal = case mKlv of
              Just klv -> klvGetLeaveValue klv leave
              Nothing  -> 0
            -- Score is in points, LeaveValue is fixed-point (1000x resolution)
            -- Convert score to same resolution: score * 1000 + leaveVal
        in Equity (fromIntegral score * 1000 + leaveVal)

      -- Update a move with computed equity
      withEquity :: Move -> Move
      withEquity m = m { moveEquity = computeEquity m }

      -- Maximum possible leave value for any rack subset (conservative upper bound)
      -- This is needed because shadow bounds don't include leave values
      -- Raw KLV values range from about -42 to +75 points
      -- In fixed-point (1000x): 75 * 1000 = 75000
      maxLeaveValue :: Equity
      maxLeaveValue = case mKlv of
        Nothing -> Equity 0
        Just _  -> Equity 75000  -- Conservative upper bound (max KLV value in 1000x resolution)

      processAnchorsWithPruning :: Move -> Equity -> [Anchor] -> Move
      processAnchorsWithPruning best bestEquity [] = best
      processAnchorsWithPruning best bestEquity (anchor:rest)
        -- Pruning: if this anchor's upper bound + max leave can't beat our best equity, we're done
        -- We need to add maxLeaveValue because shadow doesn't compute leave bounds
        -- Anchor score is in points, convert to 1000x resolution for comparison
        | Equity (fromIntegral (anchorHighestPossibleScore anchor) * 1000) + maxLeaveValue <= bestEquity = best
        | otherwise =
            let -- Generate moves at this anchor
                dir = anchorDir anchor
                row = anchorRow anchor
                col = anchorCol anchor
                lastAnchorCol = anchorLastAnchorCol anchor

                -- Generate moves based on direction
                moves = case dir of
                  Horizontal ->
                    genMovesAtAnchor cfg kwg ld boardH rack rackCrossSet
                                     Horizontal row col lastAnchorCol
                  Vertical ->
                    -- For vertical moves, use pre-computed transposed board
                    -- In transposed board, row and col are swapped
                    let vMoves = genMovesAtAnchor cfg kwg ld boardVTrans rack rackCrossSet
                                                  Horizontal col row lastAnchorCol
                    in map transposeMove vMoves

                -- Compute equity for each move and find the best
                movesWithEquity = map withEquity moves

                -- Find best move from this anchor by equity
                (newBest, newBestEquity) = case movesWithEquity of
                  [] -> (best, bestEquity)
                  _  -> let topMove = maximumBy (comparing moveEquity) movesWithEquity
                            topEquity = moveEquity topMove
                        in if topEquity > bestEquity
                           then (topMove, topEquity)
                           else (best, bestEquity)

            in processAnchorsWithPruning newBest newBestEquity rest

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
          _  -> let bestExchange = maximumBy (comparing moveEquity) exchangeMoves
                    bestExchangeEquity = moveEquity bestExchange
                in if bestExchangeEquity > passEquity
                   then (bestExchange, bestExchangeEquity)
                   else (passWithEquity, passEquity)

  in processAnchorsWithPruning initialBest initialBestEquity anchors
  where
    maximumBy _ [] = Move Pass 0 0 Horizontal [] 0 0 (Equity 0)
    maximumBy cmp (x:xs) = go x xs
      where go best [] = best
            go best (y:ys) = go (if cmp y best == GT then y else best) ys

-- | Generate moves at a single anchor position
genMovesAtAnchor :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> Word64
                 -> Direction -> Int -> Int -> Int -> [Move]
genMovesAtAnchor cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol =
  let initialUniquePlay = case dir of
        Horizontal -> True
        Vertical -> False
  in recursiveGen cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                  (gaddagRoot kwg) anchorCol anchorCol anchorCol
                  initialUniquePlay 0 1 0 0 IM.empty

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
                                     IM.empty  -- empty strip

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
             col leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore tilesPlayed strip
  | nodeIdx == 0 = []
  | col < 0 || col >= boardDim = []
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
               newStrip = IM.insert col playedThroughML strip
           in if nextNodeIdx == 0 && not accepts
              then []
              else goOn cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                        nextNodeIdx accepts col currentLetter
                        leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore tilesPlayed newStrip

         else -- Try placing from rack
           if not (rackIsEmpty rack) && ((possibleLettersHere .&. rackCrossSet) /= 0)
           then tryAllLettersAtNode cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                                    nodeIdx col leftstrip rightstrip uniquePlay mainWordScore wordMult
                                    crossScore possibleLettersHere tilesPlayed strip
           else []

-- | Try all valid letters at node (matches MAGPIE's letter iteration loop)
tryAllLettersAtNode :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> Word64
                    -> Direction -> Int -> Int -> Int -> Word32
                    -> Int -> Int -> Int -> Bool -> Int -> Int -> Int -> Word64 -> Int -> Strip -> [Move]
tryAllLettersAtNode cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol nodeIdx
                    col leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore possibleLetters tilesPlayed strip =
  go nodeIdx
  where
    go i =
      let node = getNode kwg i
          tile = nodeTile node
          ml = MachineLetter (fromIntegral tile)
          numberOfMl = rackGetCount ml rack
          hasBlank = rackHasLetter blankML rack
          accepts = nodeAccepts node
          nextNodeIdx = nodeArcIndex node
          isEnd = nodeIsEnd node
          rest = if isEnd then [] else go (i + 1)

          canPlace = tile /= 0 &&
                     (numberOfMl > 0 || hasBlank) &&
                     testBit possibleLetters (fromIntegral tile)

          movesForLetter = if not canPlace
                           then []
                           else -- Try natural tile first
                                let naturalMoves = if numberOfMl > 0
                                                   then let newRack = rackTakeLetter ml rack
                                                            newStrip = IM.insert col ml strip
                                                        in goOn cfg kwg ld board newRack rackCrossSet dir row
                                                                anchorCol lastAnchorCol nextNodeIdx accepts
                                                                col ml leftstrip rightstrip uniquePlay
                                                                mainWordScore wordMult crossScore (tilesPlayed + 1) newStrip
                                                   else []
                                    -- Try blank (ALWAYS if available, even if natural was used)
                                    blankMoves = if hasBlank
                                                 then let newRack = rackTakeLetter blankML rack
                                                          blankedML = blankLetter ml  -- Blank representing this letter
                                                          newStrip = IM.insert col blankedML strip
                                                      in goOn cfg kwg ld board newRack rackCrossSet dir row
                                                              anchorCol lastAnchorCol nextNodeIdx accepts
                                                              col blankedML leftstrip rightstrip uniquePlay
                                                              mainWordScore wordMult crossScore (tilesPlayed + 1) newStrip
                                                 else []
                                in naturalMoves ++ blankMoves
      in movesForLetter ++ rest

-- | Continue generation after placing/traversing a letter (matches MAGPIE's go_on)
goOn :: MoveGenConfig -> KWG -> LetterDistribution -> Board -> Rack -> Word64
     -> Direction -> Int -> Int -> Int -> Word32 -> Bool
     -> Int -> MachineLetter  -- current_col, letter placed
     -> Int -> Int -> Bool -> Int -> Int -> Int -> Int  -- leftstrip, rightstrip, uniquePlay, scores, tilesPlayed
     -> Strip
     -> [Move]
goOn cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol newNodeIdx accepts
     currentCol letterPlaced leftstrip rightstrip uniquePlay mainWordScore wordMult crossScore tilesPlayed strip =
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
           recordedMoves = if accepts && noLetterDirectlyLeft && noLetterRightOfAnchor &&
                              playIsNonemptyAndNonduplicate tilesPlayed newUniquePlay
                           then [buildMove cfg board row newLeftstrip rightstrip newMainScore newWordMult newCrossScore tilesPlayed strip]
                           else []

           -- Continue left (if node continues and we haven't hit lastAnchorCol)
           leftMoves = if newNodeIdx /= 0 && currentCol > 0 && (currentCol - 1) /= lastAnchorCol
                       then recursiveGen cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                                        newNodeIdx (currentCol - 1) newLeftstrip rightstrip
                                        newUniquePlay newMainScore newWordMult newCrossScore tilesPlayed strip
                       else []

           -- Cross separator to go right (if valid)
           separatorNode = getSeparatorArc kwg newNodeIdx
           rightMoves = if separatorNode /= 0 && noLetterDirectlyLeft && anchorCol < boardDim - 1
                        then recursiveGen cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                                         separatorNode (anchorCol + 1) newLeftstrip rightstrip
                                         newUniquePlay newMainScore newWordMult newCrossScore tilesPlayed strip
                        else []

       in recordedMoves ++ leftMoves ++ rightMoves

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
           recordedMoves = if accepts && noLetterDirectlyRight &&
                              playIsNonemptyAndNonduplicate tilesPlayed newUniquePlay
                           then [buildMove cfg board row leftstrip newRightstrip newMainScore newWordMult newCrossScore tilesPlayed strip]
                           else []

           -- Continue right
           rightMoves = if newNodeIdx /= 0 && currentCol < boardDim - 1
                        then recursiveGen cfg kwg ld board rack rackCrossSet dir row anchorCol lastAnchorCol
                                         newNodeIdx (currentCol + 1) leftstrip newRightstrip
                                         newUniquePlay newMainScore newWordMult newCrossScore tilesPlayed strip
                        else []

       in recordedMoves ++ rightMoves

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
                   , let ml = IM.findWithDefault (MachineLetter 0) col strip
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

-- | Compute leave value from remaining rack
-- Returns fixed-point Int32 (1000x resolution), divide by 1000 for actual value
computeLeaveValue :: Maybe KLV -> Rack -> LeaveValue
computeLeaveValue Nothing _ = 0
computeLeaveValue (Just klv) rack = klvGetLeaveValue klv rack

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
