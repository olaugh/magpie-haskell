{-# LANGUAGE BangPatterns #-}

-- | Game board representation and cross-set computation
module Magpie.Board
  ( Board(..)
  , emptyBoard
  , standardBoard
  , getLetter
  , setLetter
  , getSquare
  , setSquare
  , isEmpty
  , isOnBoard

    -- * Cross-set computation
  , computeCrossSets
  , getCrossSet
  , getCrossScore
  , getLeftExtensionSet
  , getRightExtensionSet

    -- * Anchors
  , computeAnchors
  , isAnchor

    -- * Board display
  , showBoard
  , bonusColor
  , bonusFullWidth
  , mlToFullWidth
  , fullWidthCols

    -- * Board updates
  , placeTiles
  , getTilesPlayed
  , setRow
  , stringToCrossSet
  , trivialCrossSet

    -- * CGP parsing
  , loadCGP
  , transpose
  ) where

import Magpie.Types
import Magpie.KWG (KWG, dawgRoot, gaddagRoot, getNextNodeIndex, checkAccepts, getLetterSet)
import Magpie.LetterDistribution

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word32, Word64)
import Data.Bits (setBit)
import Control.Monad (forM_)
import Control.Monad.ST (runST)

-- | The game board - flat vector with manual indexing for performance
-- Uses row * boardDim + col indexing (row-major order)
data Board = Board
  { boardSquares :: !(V.Vector Square)  -- Flat vector of 225 squares
  , boardDim_    :: !Int
  } deriving (Show)

-- | Convert (row, col) to flat index
{-# INLINE boardIndex #-}
boardIndex :: Int -> Int -> Int -> Int
boardIndex dim row col = row * dim + col

-- | Standard 15x15 Scrabble board layout (flat vector, row-major)
standardBonuses :: V.Vector BonusSquare
standardBonuses = V.fromList $ concat
  [ [tw, ns, ns, dl, ns, ns, ns, tw, ns, ns, ns, dl, ns, ns, tw]
  , [ns, dw, ns, ns, ns, tl, ns, ns, ns, tl, ns, ns, ns, dw, ns]
  , [ns, ns, dw, ns, ns, ns, dl, ns, dl, ns, ns, ns, dw, ns, ns]
  , [dl, ns, ns, dw, ns, ns, ns, dl, ns, ns, ns, dw, ns, ns, dl]
  , [ns, ns, ns, ns, dw, ns, ns, ns, ns, ns, dw, ns, ns, ns, ns]
  , [ns, tl, ns, ns, ns, tl, ns, ns, ns, tl, ns, ns, ns, tl, ns]
  , [ns, ns, dl, ns, ns, ns, dl, ns, dl, ns, ns, ns, dl, ns, ns]
  , [tw, ns, ns, dl, ns, ns, ns, dw, ns, ns, ns, dl, ns, ns, tw]
  , [ns, ns, dl, ns, ns, ns, dl, ns, dl, ns, ns, ns, dl, ns, ns]
  , [ns, tl, ns, ns, ns, tl, ns, ns, ns, tl, ns, ns, ns, tl, ns]
  , [ns, ns, ns, ns, dw, ns, ns, ns, ns, ns, dw, ns, ns, ns, ns]
  , [dl, ns, ns, dw, ns, ns, ns, dl, ns, ns, ns, dw, ns, ns, dl]
  , [ns, ns, dw, ns, ns, ns, dl, ns, dl, ns, ns, ns, dw, ns, ns]
  , [ns, dw, ns, ns, ns, tl, ns, ns, ns, tl, ns, ns, ns, dw, ns]
  , [tw, ns, ns, dl, ns, ns, ns, tw, ns, ns, ns, dl, ns, ns, tw]
  ]
  where
    ns = NoBonus
    dl = DoubleLetter
    tl = TripleLetter
    dw = DoubleWord
    tw = TripleWord

-- | Create an empty board with standard layout
emptyBoard :: Board
emptyBoard = standardBoard

-- | Create a standard 15x15 board
standardBoard :: Board
standardBoard = Board
  { boardSquares = V.generate (boardDim * boardDim) $ \i ->
      emptySquare { sqBonus = standardBonuses V.! i }
  , boardDim_ = boardDim
  }

-- | Check if position is on board
{-# INLINE isOnBoard #-}
isOnBoard :: Board -> Row -> Col -> Bool
isOnBoard board r c =
  r >= 0 && r < boardDim_ board && c >= 0 && c < boardDim_ board

-- | Get the letter at a position
{-# INLINE getLetter #-}
getLetter :: Board -> Row -> Col -> MachineLetter
getLetter board r c
  | isOnBoard board r c = sqLetter (boardSquares board V.! boardIndex (boardDim_ board) r c)
  | otherwise = MachineLetter 0

-- | Set the letter at a position
{-# INLINE setLetter #-}
setLetter :: Board -> Row -> Col -> MachineLetter -> Board
setLetter board r c ml
  | isOnBoard board r c =
      let dim = boardDim_ board
          idx = boardIndex dim r c
          sq = boardSquares board V.! idx
          sq' = sq { sqLetter = ml }
      in board { boardSquares = boardSquares board V.// [(idx, sq')] }
  | otherwise = board

-- | Set a square at a position
{-# INLINE setSquare #-}
setSquare :: Board -> Row -> Col -> Square -> Board
setSquare board r c sq
  | isOnBoard board r c =
      let dim = boardDim_ board
          idx = boardIndex dim r c
      in board { boardSquares = boardSquares board V.// [(idx, sq)] }
  | otherwise = board

-- | Get the full square at a position
{-# INLINE getSquare #-}
getSquare :: Board -> Row -> Col -> Square
getSquare board r c
  | isOnBoard board r c = boardSquares board V.! boardIndex (boardDim_ board) r c
  | otherwise = emptySquare

-- | Check if a square is empty
{-# INLINE isEmpty #-}
isEmpty :: Board -> Row -> Col -> Bool
isEmpty board r c = unML (getLetter board r c) == 0

-- | Compute cross-sets for all squares (for a given direction)
-- A cross-set indicates which letters can legally be played at a square
-- considering only the perpendicular word formed
-- Also computes extension sets for shadow algorithm
computeCrossSets :: KWG -> LetterDistribution -> Direction -> Board -> Board
computeCrossSets kwg ld dir board = runST $ do
  -- Create mutable copy of flat vector
  squares <- V.thaw (boardSquares board)
  let dim = boardDim_ board

  -- Iterate over all (row, col) positions
  forM_ [0 .. dim - 1] $ \r -> do
    forM_ [0 .. dim - 1] $ \c -> do
      let idx = boardIndex dim r c
      sq <- MV.read squares idx

      if unML (sqLetter sq) /= 0
        then return ()  -- Square is occupied, no cross-set needed
        else do
          -- Find tiles in cross direction and compute valid letters + extension sets
          let (crossSet, crossScore, leftExt, rightExt) =
                computeCrossSetAndExtensions kwg ld board dir r c
          MV.write squares idx $ sq
            { sqCrossSet = crossSet
            , sqCrossScore = crossScore
            , sqLeftExtensionSet = leftExt
            , sqRightExtensionSet = rightExt
            }

  squares' <- V.freeze squares
  return board { boardSquares = squares' }

-- | Compute cross-set and extension sets for a single square
-- Returns (crossSet, crossScore, leftExtensionSet, rightExtensionSet)
computeCrossSetAndExtensions :: KWG -> LetterDistribution -> Board -> Direction -> Row -> Col -> (Word64, Int, Word64, Word64)
computeCrossSetAndExtensions kwg ld board dir r c =
  let -- Get perpendicular direction
      crossDir = otherDirection dir
      (dr, dc) = case crossDir of
        Horizontal -> (0, 1)
        Vertical   -> (1, 0)

      -- Find start of perpendicular word
      findStart row col
        | not (isOnBoard board (row - dr) (col - dc)) = (row, col)
        | isEmpty board (row - dr) (col - dc) = (row, col)
        | otherwise = findStart (row - dr) (col - dc)

      -- Find end of perpendicular word
      findEnd row col
        | not (isOnBoard board (row + dr) (col + dc)) = (row, col)
        | isEmpty board (row + dr) (col + dc) = (row, col)
        | otherwise = findEnd (row + dr) (col + dc)

      (startR, startC) = findStart r c
      (endR, endC) = findEnd r c

      -- Check if there are any perpendicular tiles
      hasPerp = (startR, startC) /= (r, c) || (endR, endC) /= (r, c)
      trivial = allLettersMask (ldSize ld)

  in if not hasPerp
     then (trivial, 0, trivial, trivial)  -- No perpendicular constraint, all extensions valid
     else
       let beforeLetters = collectLetters board startR startC r c dr dc
           afterLetters = collectLetters board r c (endR + dr) (endC + dc) dr dc
           scoreML ml = if isBlank ml then 0 else ldScore ld ml
           perpScore = sum [scoreML ml | ml <- beforeLetters ++ afterLetters]
           crossSet = computeValidLetters kwg (map unblankLetter beforeLetters) (map unblankLetter afterLetters) (ldSize ld)

           -- Compute extension sets using GADDAG traversal
           -- For tiles to the left: traverse backward, get letter sets
           gRoot = gaddagRoot kwg
           (leftExt, rightExt) = computeExtensionSetsFromTiles kwg gRoot
                                   (map unblankLetter beforeLetters)
                                   (map unblankLetter afterLetters)
                                   (ldSize ld)

       in (crossSet, perpScore, leftExt, rightExt)

-- | Compute extension sets from existing tiles using GADDAG
-- beforeTiles: tiles to the "left" (earlier in reading direction)
-- afterTiles: tiles to the "right" (later in reading direction)
computeExtensionSetsFromTiles :: KWG -> Word32 -> [MachineLetter] -> [MachineLetter] -> Int -> (Word64, Word64)
computeExtensionSetsFromTiles kwg gRoot beforeTiles afterTiles distSize =
  let trivial = allLettersMask distSize

      -- For tiles before (to the left), traverse backward in GADDAG
      -- GADDAG stores words reversed up to a point, so we traverse in reverse
      leftNode = case beforeTiles of
        [] -> gRoot
        _  -> foldl (getNextNodeIndex kwg) gRoot (reverse beforeTiles)

      (leftExtSet, _) = if leftNode /= 0
                        then getLetterSet kwg leftNode
                        else (0, 0)

      -- For the right extension, we need to traverse through the separator
      -- The separator (arc 0) leads to the suffix part of the word
      sepNode = if leftNode /= 0
                then getNextNodeIndex kwg leftNode (MachineLetter 0)  -- Separator
                else 0

      -- If we have tiles after, traverse through them after separator
      rightNode = case afterTiles of
        [] -> sepNode
        _  -> if sepNode /= 0
              then foldl (getNextNodeIndex kwg) sepNode afterTiles
              else 0

      (rightExtSet, _) = if rightNode /= 0
                         then getLetterSet kwg rightNode
                         else (0, 0)

      -- If no valid left extensions found, use trivial (any letter)
      finalLeftExt = if leftExtSet == 0 && null beforeTiles then trivial else leftExtSet
      -- If no valid right extensions found, use trivial
      finalRightExt = if rightExtSet == 0 && null afterTiles then trivial else rightExtSet

  in (addBlankBit finalLeftExt, addBlankBit finalRightExt)
  where
    -- If any letter is valid, blank is also valid
    addBlankBit :: Word64 -> Word64
    addBlankBit 0 = 0
    addBlankBit s = setBit s 0

-- | Collect letters between two positions (exclusive of end)
-- Returns the original letters (with blank bits intact)
collectLetters :: Board -> Row -> Col -> Row -> Col -> Int -> Int -> [MachineLetter]
collectLetters board startR startC endR endC dr dc = go startR startC
  where
    go r c
      | r == endR && c == endC = []
      | otherwise =
          let ml = getLetter board r c
          in if unML ml == 0
             then go (r + dr) (c + dc)  -- Skip the empty square we're computing for
             else ml : go (r + dr) (c + dc)  -- Keep original (with blank bit)

-- | Compute which letters form valid words with given prefix and suffix
-- For a cross-set at position P with tiles before and after:
--   beforeLetters = tiles to the left of P (collected left to right)
--   afterLetters = tiles to the right of P (collected left to right)
-- We check if: beforeLetters ++ [candidateLetter] ++ afterLetters is a valid word
computeValidLetters :: KWG -> [MachineLetter] -> [MachineLetter] -> Int -> Word64
computeValidLetters kwg prefix suffix distSize =
  let root = dawgRoot kwg
      -- Traverse prefix
      prefixNode = foldl (getNextNodeIndex kwg) root prefix
      letterBits = if prefixNode == 0 && not (null prefix)
         then 0  -- Prefix is not valid
         else foldr (\ml acc ->
               let valid = case suffix of
                     -- Empty suffix: word is prefix ++ [ml], check if ml accepts at prefixNode
                     [] -> checkAccepts kwg prefixNode ml
                     -- Non-empty suffix: traverse ml then check suffix
                     _  -> let node = getNextNodeIndex kwg prefixNode ml
                           in checkSuffixFromNode kwg node suffix
               in if valid
                  then setBit acc (fromIntegral (unML ml))
                  else acc
             ) 0 [MachineLetter i | i <- [1 .. fromIntegral distSize - 1]]
  -- If any letters are valid, blanks (bit 0) are also valid
  in if letterBits /= 0 then setBit letterBits 0 else 0

-- | Check if suffix forms a valid word ending from current node
-- Called after already traversing at least one letter (the candidate)
checkSuffixFromNode :: KWG -> Word32 -> [MachineLetter] -> Bool
checkSuffixFromNode _ 0 _ = False  -- Invalid path
checkSuffixFromNode kwg node [ml] = checkAccepts kwg node ml  -- Last letter: check acceptance
checkSuffixFromNode kwg node (ml:rest) =
  let next = getNextNodeIndex kwg node ml
  in checkSuffixFromNode kwg next rest
checkSuffixFromNode _ _ [] = False  -- Shouldn't happen with our call pattern

-- | All letters mask (bits 1 through distSize-1 set)
allLettersMask :: Int -> Word64
allLettersMask distSize = foldr (\i acc -> setBit acc i) 0 [1 .. distSize - 1]

-- | Get cross-set for a square
getCrossSet :: Board -> Row -> Col -> Word64
getCrossSet board r c = sqCrossSet (getSquare board r c)

-- | Get cross-score for a square
getCrossScore :: Board -> Row -> Col -> Int
getCrossScore board r c = sqCrossScore (getSquare board r c)

-- | Get left extension set for a square
getLeftExtensionSet :: Board -> Row -> Col -> Word64
getLeftExtensionSet board r c = sqLeftExtensionSet (getSquare board r c)

-- | Get right extension set for a square
getRightExtensionSet :: Board -> Row -> Col -> Word64
getRightExtensionSet board r c = sqRightExtensionSet (getSquare board r c)

-- | Compute anchors for the board
-- An anchor is an empty square adjacent to a filled square
computeAnchors :: Board -> Board
computeAnchors board = runST $ do
  squares <- V.thaw (boardSquares board)
  let dim = boardDim_ board

  -- Check if center is empty (first move)
  let centerR = dim `div` 2
      centerC = dim `div` 2
      centerEmpty = isEmpty board centerR centerC

  forM_ [0 .. dim - 1] $ \r -> do
    forM_ [0 .. dim - 1] $ \c -> do
      let idx = boardIndex dim r c
      sq <- MV.read squares idx
      let isAnch = if unML (sqLetter sq) /= 0
                   then False
                   else if centerEmpty && r == centerR && c == centerC
                        then True  -- Center is anchor for first move
                        else hasAdjacentTile board r c
      MV.write squares idx $ sq { sqIsAnchor = isAnch }

  squares' <- V.freeze squares
  return board { boardSquares = squares' }

-- | Check if a square has an adjacent tile
hasAdjacentTile :: Board -> Row -> Col -> Bool
hasAdjacentTile board r c =
  any (not . isEmpty board r . (+c)) [-1, 1] ||
  any (not . flip (isEmpty board) c . (+r)) [-1, 1]

-- | Check if a square is an anchor
isAnchor :: Board -> Row -> Col -> Bool
isAnchor board r c = sqIsAnchor (getSquare board r c)

-- | Place tiles on the board (for a move)
placeTiles :: Board -> Row -> Col -> Direction -> [MachineLetter] -> Board
placeTiles board startR startC dir tiles = go board startR startC tiles
  where
    (dr, dc) = case dir of
      Horizontal -> (0, 1)
      Vertical   -> (1, 0)

    go b _ _ [] = b
    go b r c (ml:rest)
      | unML ml == 0 =  -- Play-through marker, skip
          go b (r + dr) (c + dc) rest
      | otherwise =
          let b' = setLetter b r c ml
          in go b' (r + dr) (c + dc) rest

-- | Get number of tiles on the board
getTilesPlayed :: Board -> Int
getTilesPlayed board =
  sum [ if isEmpty board r c then 0 else 1
      | r <- [0 .. boardDim_ board - 1]
      , c <- [0 .. boardDim_ board - 1]
      ]

-- | Set a row from a string (for testing)
-- Spaces are empty squares, letters are tiles
-- Lowercase letters are blanks
setRow :: LetterDistribution -> Board -> Int -> String -> Board
setRow ld board row content = go (clearRow board row) 0 content
  where
    clearRow b r = foldr (\c b' -> setLetter b' r c (MachineLetter 0)) b [0..boardDim_ b - 1]

    go b _ [] = b
    go b col (c:rest)
      | col >= boardDim_ b = b
      | c == ' ' = go b (col + 1) rest
      | otherwise =
          let ml = ldCharToML ld c
          in go (setLetter b row col ml) (col + 1) rest

-- | Convert a string of letters to a cross set bitmask
-- "?ABC" means blank, A, B, C are allowed
-- Empty string means nothing allowed
-- For trivial cross set, all letters 1..26 are allowed
stringToCrossSet :: LetterDistribution -> String -> Word64
stringToCrossSet ld str = foldr addLetter 0 str
  where
    addLetter c acc = setBit acc (fromIntegral $ unML (ldCharToML ld c))

-- | Trivial cross set (all letters allowed)
trivialCrossSet :: Int -> Word64
trivialCrossSet distSize = foldr (\i acc -> setBit acc i) 0 [1 .. distSize - 1]

-- | ANSI color codes
colorReset, colorBold, colorCyan, colorMagenta, colorBlue, colorRed :: String
colorReset = "\x1b[0m"
colorBold = "\x1b[1m"
colorCyan = "\x1b[1;36m"      -- DLS
colorMagenta = "\x1b[1;35m"   -- DWS
colorBlue = "\x1b[1;34m"      -- TLS
colorRed = "\x1b[1;31m"       -- TWS

-- | Fullwidth column labels (Ａ-Ｏ)
fullWidthCols :: [String]
fullWidthCols = ["Ａ","Ｂ","Ｃ","Ｄ","Ｅ","Ｆ","Ｇ","Ｈ","Ｉ","Ｊ","Ｋ","Ｌ","Ｍ","Ｎ","Ｏ"]

-- | Fullwidth bonus square characters
bonusFullWidth :: BonusSquare -> String
bonusFullWidth NoBonus = "　"       -- Ideographic space
bonusFullWidth DoubleLetter = "＇"  -- Fullwidth apostrophe
bonusFullWidth TripleLetter = "＂"  -- Fullwidth quotation
bonusFullWidth DoubleWord = "－"    -- Fullwidth hyphen
bonusFullWidth TripleWord = "＝"    -- Fullwidth equals

-- | Bonus square color
bonusColor :: BonusSquare -> String
bonusColor NoBonus = colorReset
bonusColor DoubleLetter = colorCyan
bonusColor TripleLetter = colorBlue
bonusColor DoubleWord = colorMagenta
bonusColor TripleWord = colorRed

-- | Fullwidth uppercase letters (Ａ-Ｚ)
fullWidthUpper :: Char -> String
fullWidthUpper c
  | c >= 'A' && c <= 'Z' = [toEnum (fromEnum 'Ａ' + fromEnum c - fromEnum 'A')]
  | otherwise = [c]

-- | Fullwidth lowercase letters (ａ-ｚ)
fullWidthLower :: Char -> String
fullWidthLower c
  | c >= 'a' && c <= 'z' = [toEnum (fromEnum 'ａ' + fromEnum c - fromEnum 'a')]
  | otherwise = [c]

-- | Convert machine letter to fullwidth string
mlToFullWidth :: LetterDistribution -> MachineLetter -> String
mlToFullWidth ld ml
  | unML ml == 0 = "？"
  | isBlank ml = fullWidthLower (ldToChar ld (unblankLetter ml))
  | otherwise = fullWidthUpper (ldToChar ld ml)

-- | Display the board as a string (MAGPIE pretty style with colors)
showBoard :: LetterDistribution -> Board -> String
showBoard ld board =
  unlines $
    -- Column headers (fullwidth)
    ["   " ++ concat fullWidthCols] ++
    -- Top border
    ["  ┏" ++ replicate (boardDim * 2) '━' ++ "┓"] ++
    -- Board rows
    [ show2 (r + 1) ++ "┃" ++
      concatMap (\c ->
        let sq = getSquare board r c
            ml = sqLetter sq
        in if unML ml == 0
           then bonusColor (sqBonus sq) ++ bonusFullWidth (sqBonus sq) ++ colorReset
           else colorReset ++ colorBold ++ mlToFullWidth ld ml ++ colorReset
      ) [0 .. boardDim - 1] ++ "┃"
    | r <- [0 .. boardDim - 1]
    ] ++
    -- Bottom border
    ["  ┗" ++ replicate (boardDim * 2) '━' ++ "┛"]
  where
    show2 n = if n < 10 then " " ++ show n else show n

-- | Parse a CGP board string and load it onto the board
-- CGP format: rows separated by /, numbers indicate empty squares
-- Example: "7ZEP1F3/1FLUKY3R1R3/..."
loadCGP :: LetterDistribution -> String -> Board
loadCGP ld cgp =
  let boardPart = takeWhile (/= ' ') cgp  -- Take board part before rack/scores
      rows = splitOn '/' boardPart
  in foldl (\b (row, rowStr) -> loadRow ld b row rowStr) standardBoard (zip [0..] rows)
  where
    splitOn _ [] = []
    splitOn c s = let (w, rest) = break (== c) s
                  in w : case rest of
                           [] -> []
                           (_:xs) -> splitOn c xs

-- | Load a single row from CGP format
loadRow :: LetterDistribution -> Board -> Int -> String -> Board
loadRow ld board row str = go board 0 str
  where
    go b _ [] = b
    go b col (c:rest)
      | col >= boardDim_ b = b
      | c >= '0' && c <= '9' =
          -- Number means skip that many empty squares
          -- Check if next char is also a digit (for two-digit numbers like 15)
          case rest of
            (d:remaining) | d >= '0' && d <= '9' ->
              let count = (fromEnum c - fromEnum '0') * 10 + (fromEnum d - fromEnum '0')
              in go b (col + count) remaining
            _ ->
              let count = fromEnum c - fromEnum '0'
              in go b (col + count) rest
      | otherwise =
          let ml = ldCharToML ld c
          in go (setLetter b row col ml) (col + 1) rest

-- | Transpose the board (swap rows and columns)
transpose :: Board -> Board
transpose board =
  let dim = boardDim_ board
      -- Create new flat vector with transposed indexing
      newSquares = V.generate (dim * dim) $ \i ->
        let r = i `div` dim
            c = i `mod` dim
            -- Swap r and c for transposition
        in boardSquares board V.! boardIndex dim c r
  in board { boardSquares = newSquares }
