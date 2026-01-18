{-# LANGUAGE BangPatterns #-}

-- | Game state and logic
module Magpie.Game
  ( Game(..)
  , Player(..)
  , newGame
  , currentPlayer
  , makeMove
  , isGameOver
  , finalScores
  , showGame
  , autoplay
  , AutoplayResult(..)
  ) where

import Magpie.Types
import Magpie.Board
import Magpie.LetterDistribution
import Magpie.KWG
import Magpie.MoveGen

import System.Random (StdGen, randomR)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- | A player in the game
data Player = Player
  { playerName  :: !String
  , playerRack  :: !Rack
  , playerScore :: !Int
  } deriving (Show)

-- | The game state
data Game = Game
  { gameBoard      :: !Board
  , gamePlayers    :: ![Player]
  , gameCurrentIdx :: !Int
  , gameBag        :: !Rack          -- ^ Tiles remaining in bag
  , gameLD         :: !LetterDistribution
  , gameKWG        :: !KWG
  , gamePasses     :: !Int           -- ^ Consecutive passes
  , gameOver       :: !Bool
  } deriving (Show)

-- | Create a new game
newGame :: LetterDistribution -> KWG -> StdGen -> (Game, StdGen)
newGame ld kwg gen =
  let distSize = ldSize ld
      initialBag = foldr (\ml r -> foldr (const (rackAddLetter ml)) r [1..ldCount ld ml])
                         (emptyRack distSize)
                         [MachineLetter i | i <- [0 .. fromIntegral distSize - 1]]

      -- Draw tiles for player 1
      (rack1, bag1, gen1) = drawTiles initialBag defaultRackSize gen distSize

      -- Draw tiles for player 2
      (rack2, bag2, gen2) = drawTiles bag1 defaultRackSize gen1 distSize

      p1 = Player "Player 1" rack1 0
      p2 = Player "Player 2" rack2 0

  in (Game
      { gameBoard = standardBoard
      , gamePlayers = [p1, p2]
      , gameCurrentIdx = 0
      , gameBag = bag2
      , gameLD = ld
      , gameKWG = kwg
      , gamePasses = 0
      , gameOver = False
      }, gen2)

-- | Draw tiles from the bag
drawTiles :: Rack -> Int -> StdGen -> Int -> (Rack, Rack, StdGen)
drawTiles bag count gen distSize = go bag (emptyRack distSize) count gen
  where
    go !currentBag !rack !remaining !g
      | remaining <= 0 = (rack, currentBag, g)
      | rackTotal currentBag == 0 = (rack, currentBag, g)
      | otherwise =
          let (ml, newBag, g') = drawOneTile currentBag g distSize
              newRack = rackAddLetter ml rack
          in go newBag newRack (remaining - 1) g'

-- | Draw one tile from the bag
drawOneTile :: Rack -> StdGen -> Int -> (MachineLetter, Rack, StdGen)
drawOneTile bag gen distSize =
  let total = rackTotal bag
      (idx, gen') = randomR (0, total - 1) gen
      ml = findTileAtIndex bag idx distSize
      newBag = rackTakeLetter ml bag
  in (ml, newBag, gen')

-- | Find the tile at a given index in the bag
findTileAtIndex :: Rack -> Int -> Int -> MachineLetter
findTileAtIndex bag targetIdx distSize = go 0 0
  where
    go !ml !cumulative
      | ml >= distSize = MachineLetter 0  -- Should never happen
      | otherwise =
          let count = rackGetCount (MachineLetter $ fromIntegral ml) bag
              newCumulative = cumulative + count
          in if targetIdx < newCumulative
             then MachineLetter $ fromIntegral ml
             else go (ml + 1) newCumulative

-- | Get the current player
currentPlayer :: Game -> Player
currentPlayer game = gamePlayers game !! gameCurrentIdx game

-- | Make a move in the game
makeMove :: Game -> Move -> StdGen -> (Game, StdGen)
makeMove game move gen
  | gameOver game = (game, gen)
  | otherwise =
      case moveType move of
        Pass -> handlePass game gen
        Exchange -> handleExchange game move gen
        TilePlacement -> handlePlacement game move gen

-- | Handle a pass
handlePass :: Game -> StdGen -> (Game, StdGen)
handlePass game gen =
  let newPasses = gamePasses game + 1
      isOver = newPasses >= 6  -- 3 passes each = 6 total
      newGame' = game
        { gameCurrentIdx = (gameCurrentIdx game + 1) `mod` length (gamePlayers game)
        , gamePasses = newPasses
        , gameOver = isOver
        }
  in (newGame', gen)

-- | Handle an exchange
handleExchange :: Game -> Move -> StdGen -> (Game, StdGen)
handleExchange game move gen =
  let player = currentPlayer game
      tilesToExchange = moveTiles move
      distSize = ldSize (gameLD game)

      -- Remove tiles from rack
      rackAfterRemove = foldr rackTakeLetter (playerRack player) tilesToExchange

      -- Add tiles to bag
      bagAfterAdd = foldr rackAddLetter (gameBag game) tilesToExchange

      -- Draw new tiles
      (newRack, newBag, gen') = drawTiles bagAfterAdd (length tilesToExchange) gen distSize

      -- Combine racks
      finalRack = foldr rackAddLetter rackAfterRemove (rackToList newRack)

      newPlayer = player { playerRack = finalRack }
      newPlayers = updatePlayer (gamePlayers game) (gameCurrentIdx game) newPlayer

  in (game
      { gamePlayers = newPlayers
      , gameBag = newBag
      , gameCurrentIdx = (gameCurrentIdx game + 1) `mod` length (gamePlayers game)
      , gamePasses = 0
      }, gen')

-- | Handle a tile placement
handlePlacement :: Game -> Move -> StdGen -> (Game, StdGen)
handlePlacement game move gen =
  let player = currentPlayer game
      distSize = ldSize (gameLD game)

      -- Place tiles on board
      newBoard = placeTiles (gameBoard game) (moveRow move) (moveCol move)
                            (moveDir move) (moveTiles move)

      -- Remove played tiles from rack
      tilesUsed = filter (\ml -> unML ml /= 0) (moveTiles move)
      rackTiles = map (\ml -> if isBlank ml then MachineLetter 0 else ml) tilesUsed
      rackAfterPlay = foldr rackTakeLetter (playerRack player) rackTiles

      -- Draw new tiles
      (newRack, newBag, gen') = drawTiles (gameBag game) (length tilesUsed) gen distSize

      -- Combine racks
      finalRack = foldr rackAddLetter rackAfterPlay (rackToList newRack)

      -- Update score
      newScore = playerScore player + moveScore move

      newPlayer = player { playerRack = finalRack, playerScore = newScore }
      newPlayers = updatePlayer (gamePlayers game) (gameCurrentIdx game) newPlayer

      -- Check if game is over (player used all tiles and bag is empty)
      isOver = rackTotal finalRack == 0 && rackTotal newBag == 0

  in (game
      { gameBoard = newBoard
      , gamePlayers = newPlayers
      , gameBag = newBag
      , gameCurrentIdx = (gameCurrentIdx game + 1) `mod` length (gamePlayers game)
      , gamePasses = 0
      , gameOver = isOver
      }, gen')

-- | Update a player in the list
updatePlayer :: [Player] -> Int -> Player -> [Player]
updatePlayer players idx newPlayer =
  take idx players ++ [newPlayer] ++ drop (idx + 1) players

-- | Check if the game is over
isGameOver :: Game -> Bool
isGameOver = gameOver

-- | Calculate final scores (subtracting unplayed tiles)
finalScores :: Game -> [(String, Int)]
finalScores game =
  let ld = gameLD game
      adjustScore player =
        let rackValue = sum [ldScore ld ml | ml <- rackToList (playerRack player)]
        in playerScore player - rackValue
  in [(playerName p, adjustScore p) | p <- gamePlayers game]

-- | ANSI colors for game display
colorReset, colorGreen, colorBold :: String
colorReset = "\x1b[0m"
colorGreen = "\x1b[32m"
colorBold = "\x1b[1m"

-- | On-turn marker
onTurnMarker :: String
onTurnMarker = "➤"

-- | Display the game state (MAGPIE pretty style with info on right side)
showGame :: Game -> String
showGame game =
  let ld = gameLD game
      board = gameBoard game
      currentIdx = gameCurrentIdx game
      isOver = gameOver game
      players = gamePlayers game
      bag = gameBag game
      bagCount = rackTotal bag

      -- Player line formatting
      showPlayerLine i player =
        let marker = if i == currentIdx && not isOver
                     then colorGreen ++ onTurnMarker ++ " " ++ colorReset
                     else "  "
            rackStr = ldToString ld (rackToList (playerRack player))
            scoreStr = if i == currentIdx && not isOver
                       then colorBold ++ show (playerScore player) ++ colorReset
                       else show (playerScore player)
            padding = replicate (defaultRackSize + 2 - length rackStr) ' '
        in marker ++ playerName player ++ " " ++ rackStr ++ padding ++ scoreStr

      player0Line = showPlayerLine 0 (players !! 0)
      player1Line = showPlayerLine 1 (players !! 1)

      -- Unseen tiles display
      unseenTiles = rackToList bag
      unseenStr = "Unseen: (" ++ show bagCount ++ ")"
      -- Split unseen tiles into rows of ~20
      unseenRows = chunksOf 20 (ldToString ld unseenTiles)

      -- Get right-side content for each board row (0-14)
      rightContent row
        | row == 1  = "   " ++ unseenStr
        | row >= 3 && row - 3 < length unseenRows = "   " ++ addSpaces (unseenRows !! (row - 3))
        | otherwise = ""

      -- Add spaces between characters for unseen display
      addSpaces [] = []
      addSpaces [c] = [c]
      addSpaces (c:cs) = c : ' ' : addSpaces cs

      -- Build board rows with right-side content
      boardRowWithRight r =
        let sq c = getSquare board r c
            ml c = sqLetter (sq c)
            rowContent = concatMap (\c ->
              if unML (ml c) == 0
              then bonusColor (sqBonus (sq c)) ++ bonusFullWidth (sqBonus (sq c)) ++ colorReset
              else colorReset ++ colorBold ++ mlToFullWidth ld (ml c) ++ colorReset
              ) [0 .. boardDim - 1]
        in show2 (r + 1) ++ "┃" ++ rowContent ++ "┃" ++ rightContent r

      show2 n = if n < 10 then " " ++ show n else show n

      -- Column header line with player 0 info
      colHeaderLine = "   " ++ concat fullWidthCols ++ "  " ++ player0Line

      -- Top border with player 1 info
      topBorderLine = "  ┏" ++ replicate (boardDim * 2) '━' ++ "┓ " ++ player1Line

      -- Bottom border
      bottomBorderLine = "  ┗" ++ replicate (boardDim * 2) '━' ++ "┛"

  in unlines $
       [colHeaderLine] ++
       [topBorderLine] ++
       [boardRowWithRight r | r <- [0 .. boardDim - 1]] ++
       [bottomBorderLine]

-- | Split a list into chunks of given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Result of an autoplay game
data AutoplayResult = AutoplayResult
  { autoplayWinner    :: !String
  , autoplayScores    :: ![(String, Int)]
  , autoplayTurns     :: !Int
  , autoplayMoves     :: ![(String, Move)]  -- Player name and move
  } deriving (Show)

-- | Play a complete game automatically using static evaluation
autoplay :: Game -> StdGen -> Bool -> IO (AutoplayResult, StdGen)
autoplay initialGame initialGen verbose = go initialGame initialGen 0 []
  where
    go game gen turnCount moves
      | isGameOver game = do
          let scores = finalScores game
              winner = case sortBy (comparing (Down . snd)) scores of
                         ((name, _):_) -> name
                         [] -> "None"
          return (AutoplayResult winner scores turnCount (reverse moves), gen)
      | otherwise = do
          let player = currentPlayer game
              rack = playerRack player
              ld = gameLD game
              kwg = gameKWG game
              board = gameBoard game

              -- Generate moves and pick the best one
              allMoves = generateMoves kwg ld board rack
              bestMove = case allMoves of
                           (m:_) -> m
                           [] -> Move Pass 0 0 Horizontal [] 0 0 (Equity 0)

          when verbose $ do
            putStrLn $ showGame game
            putStrLn $ "Turn " ++ show (turnCount + 1) ++ ": " ++ playerName player
            putStrLn $ "Best move: " ++ showMoveSimple ld bestMove
            putStrLn ""

          let (game', gen') = makeMove game bestMove gen
              newMoves = (playerName player, bestMove) : moves

          go game' gen' (turnCount + 1) newMoves

    when True action = action
    when False _ = return ()

-- | Simple move display
-- Playthrough tiles (MachineLetter 0) are shown as '.'
showMoveSimple :: LetterDistribution -> Move -> String
showMoveSimple ld move =
  case moveType move of
    Pass -> "(pass)"
    Exchange -> "(exch " ++ ldToString ld (moveTiles move) ++ ")"
    TilePlacement ->
      let pos = [toEnum (fromEnum 'A' + moveCol move)] ++ show (moveRow move + 1)
          dirStr = case moveDir move of
            Horizontal -> ""
            Vertical -> " (down)"
          tilesStr = map mlToChar (moveTiles move)
          mlToChar ml = if unML ml == 0 then '.' else ldToChar ld ml
      in pos ++ dirStr ++ " " ++ tilesStr ++ " " ++ show (moveScore move)
