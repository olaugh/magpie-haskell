{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Threaded autoplay for running multiple games in parallel
module Magpie.Autoplay
  ( AutoplayConfig(..)
  , AutoplayResults(..)
  , defaultAutoplayConfig
  , runAutoplayThreaded
  , runMoveGenComparison
  ) where

import Magpie.Types
import Magpie.LetterDistribution
import Magpie.KWG
import Magpie.KLV (KLV)
import Magpie.WMP (WMP)
import Magpie.MoveGen (generateBestMove, defaultMoveGenConfig)
import Magpie.Board (getTilesPlayed)
import Magpie.Game

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (replicateM, forM_, when)
import System.Random (StdGen, mkStdGen, newStdGen)
import Text.Printf (printf)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Vector.Unboxed as VU

-- | Configuration for autoplay
data AutoplayConfig = AutoplayConfig
  { autoplayNumGames    :: !Int        -- ^ Number of games to play
  , autoplayNumThreads  :: !Int        -- ^ Number of worker threads
  , autoplaySeed        :: !Int        -- ^ Random seed for reproducibility
  , autoplayVerbose     :: !Bool       -- ^ Show individual game results
  , autoplayKLV         :: !(Maybe KLV) -- ^ Leave values for equity-based move selection
  , autoplayWMP         :: !(Maybe WMP) -- ^ Word Map for fast word existence checking
  } deriving (Show)

-- | Default autoplay configuration
defaultAutoplayConfig :: AutoplayConfig
defaultAutoplayConfig = AutoplayConfig
  { autoplayNumGames   = 100
  , autoplayNumThreads = 1
  , autoplaySeed       = 0
  , autoplayVerbose    = False
  , autoplayKLV        = Nothing
  , autoplayWMP        = Nothing
  }

-- | Aggregated results from all autoplay games
data AutoplayResults = AutoplayResults
  { totalGames       :: !Int
  , p1Wins           :: !Int
  , p2Wins           :: !Int
  , ties             :: !Int
  , totalTurns       :: !Int
  , totalP1Score     :: !Int
  , totalP2Score     :: !Int
  , totalP1Tiles     :: !Int      -- ^ Total tiles played by P1
  , totalP2Tiles     :: !Int      -- ^ Total tiles played by P2
  , totalP1Bingos    :: !Int      -- ^ 7-tile plays by P1
  , totalP2Bingos    :: !Int      -- ^ 7-tile plays by P2
  , totalP1Exchanges :: !Int      -- ^ Exchange turns for P1
  , totalP2Exchanges :: !Int      -- ^ Exchange turns for P2
  , totalP1Passes    :: !Int      -- ^ Pass turns for P1
  , totalP2Passes    :: !Int      -- ^ Pass turns for P2
  } deriving (Show)

-- | Empty results for accumulation
emptyResults :: AutoplayResults
emptyResults = AutoplayResults 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | Combine two results
combineResults :: AutoplayResults -> AutoplayResults -> AutoplayResults
combineResults a b = AutoplayResults
  { totalGames       = totalGames a + totalGames b
  , p1Wins           = p1Wins a + p1Wins b
  , p2Wins           = p2Wins a + p2Wins b
  , ties             = ties a + ties b
  , totalTurns       = totalTurns a + totalTurns b
  , totalP1Score     = totalP1Score a + totalP1Score b
  , totalP2Score     = totalP2Score a + totalP2Score b
  , totalP1Tiles     = totalP1Tiles a + totalP1Tiles b
  , totalP2Tiles     = totalP2Tiles a + totalP2Tiles b
  , totalP1Bingos    = totalP1Bingos a + totalP1Bingos b
  , totalP2Bingos    = totalP2Bingos a + totalP2Bingos b
  , totalP1Exchanges = totalP1Exchanges a + totalP1Exchanges b
  , totalP2Exchanges = totalP2Exchanges a + totalP2Exchanges b
  , totalP1Passes    = totalP1Passes a + totalP1Passes b
  , totalP2Passes    = totalP2Passes a + totalP2Passes b
  }

-- | Result of a single game (for aggregation)
data SingleGameResult = SingleGameResult
  { gameWinner      :: !Int      -- ^ 1 for P1, 2 for P2, 0 for tie
  , gameTurns       :: !Int
  , gameP1Score     :: !Int
  , gameP2Score     :: !Int
  , gameP1Tiles     :: !Int
  , gameP2Tiles     :: !Int
  , gameP1Bingos    :: !Int
  , gameP2Bingos    :: !Int
  , gameP1Exchanges :: !Int
  , gameP2Exchanges :: !Int
  , gameP1Passes    :: !Int
  , gameP2Passes    :: !Int
  } deriving (Show)

-- | Convert a single game result to AutoplayResults for aggregation
singleToResults :: SingleGameResult -> AutoplayResults
singleToResults SingleGameResult{..} = AutoplayResults
  { totalGames       = 1
  , p1Wins           = if gameWinner == 1 then 1 else 0
  , p2Wins           = if gameWinner == 2 then 1 else 0
  , ties             = if gameWinner == 0 then 1 else 0
  , totalTurns       = gameTurns
  , totalP1Score     = gameP1Score
  , totalP2Score     = gameP2Score
  , totalP1Tiles     = gameP1Tiles
  , totalP2Tiles     = gameP2Tiles
  , totalP1Bingos    = gameP1Bingos
  , totalP2Bingos    = gameP2Bingos
  , totalP1Exchanges = gameP1Exchanges
  , totalP2Exchanges = gameP2Exchanges
  , totalP1Passes    = gameP1Passes
  , totalP2Passes    = gameP2Passes
  }

-- | Shared state for worker threads
data SharedState = SharedState
  { nextGameNum     :: !(TVar Int)         -- ^ Next game number to play
  , gamesCompleted  :: !(TVar Int)         -- ^ Games completed so far
  , aggregateResults :: !(TVar AutoplayResults) -- ^ Running totals
  }

-- | Run autoplay with multiple threads
runAutoplayThreaded :: KWG -> LetterDistribution -> AutoplayConfig -> IO AutoplayResults
runAutoplayThreaded kwg ld config@AutoplayConfig{..} = do
  -- Initialize shared state
  nextGame <- newTVarIO 0
  completed <- newTVarIO 0
  results <- newTVarIO emptyResults
  let shared = SharedState nextGame completed results

  -- Track timing
  startTime <- getCurrentTime

  -- Create progress printer (only in non-verbose mode)
  progressVar <- newTVarIO (0 :: Int)

  -- Spawn worker threads
  threadIds <- replicateM autoplayNumThreads $ do
    forkIO $ workerThread kwg ld config shared progressVar

  -- Progress reporting thread
  progressThread <- if not autoplayVerbose
    then forkIO $ progressReporter autoplayNumGames completed
    else return =<< myThreadId  -- Dummy thread ID

  -- Wait for all workers to complete
  forM_ threadIds $ \_ -> do
    atomically $ do
      done <- readTVar completed
      when (done < autoplayNumGames) retry

  -- Wait a bit for final progress update
  threadDelay 10000

  -- Kill progress thread if we started one
  when (not autoplayVerbose) $ killThread progressThread

  endTime <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime endTime startTime) :: Double

  -- Get final results
  finalResults <- atomically $ readTVar results

  -- Print summary
  putStrLn ""
  putStrLn $ replicate 50 '='
  putStrLn "Autoplay Results"
  putStrLn $ replicate 50 '='
  printf "Games:          %d\n" (totalGames finalResults)
  printf "Threads:        %d\n" autoplayNumThreads
  printf "Seed:           %d\n" autoplaySeed
  printf "Time:           %.2f seconds\n" elapsed
  printf "Games/sec:      %.1f\n" (fromIntegral autoplayNumGames / elapsed)
  putStrLn ""

  printf "P1 wins:        %d (%.1f%%)\n"
         (p1Wins finalResults)
         (100 * fromIntegral (p1Wins finalResults) / fromIntegral autoplayNumGames :: Double)
  printf "P2 wins:        %d (%.1f%%)\n"
         (p2Wins finalResults)
         (100 * fromIntegral (p2Wins finalResults) / fromIntegral autoplayNumGames :: Double)
  printf "Ties:           %d (%.1f%%)\n"
         (ties finalResults)
         (100 * fromIntegral (ties finalResults) / fromIntegral autoplayNumGames :: Double)
  putStrLn ""

  let avgTurns = fromIntegral (totalTurns finalResults) / fromIntegral autoplayNumGames :: Double
      avgP1Score = fromIntegral (totalP1Score finalResults) / fromIntegral autoplayNumGames :: Double
      avgP2Score = fromIntegral (totalP2Score finalResults) / fromIntegral autoplayNumGames :: Double
      avgP1Tiles = fromIntegral (totalP1Tiles finalResults) / fromIntegral autoplayNumGames :: Double
      avgP2Tiles = fromIntegral (totalP2Tiles finalResults) / fromIntegral autoplayNumGames :: Double
      avgP1Bingos = fromIntegral (totalP1Bingos finalResults) / fromIntegral autoplayNumGames :: Double
      avgP2Bingos = fromIntegral (totalP2Bingos finalResults) / fromIntegral autoplayNumGames :: Double
      avgP1Exchanges = fromIntegral (totalP1Exchanges finalResults) / fromIntegral autoplayNumGames :: Double
      avgP2Exchanges = fromIntegral (totalP2Exchanges finalResults) / fromIntegral autoplayNumGames :: Double
      avgP1Passes = fromIntegral (totalP1Passes finalResults) / fromIntegral autoplayNumGames :: Double
      avgP2Passes = fromIntegral (totalP2Passes finalResults) / fromIntegral autoplayNumGames :: Double

  printf "Avg turns:      %.1f\n" avgTurns
  printf "Avg P1 score:   %.1f\n" avgP1Score
  printf "Avg P2 score:   %.1f\n" avgP2Score
  printf "Avg P1 tiles:   %.1f\n" avgP1Tiles
  printf "Avg P2 tiles:   %.1f\n" avgP2Tiles
  printf "Avg P1 bingos:  %.2f\n" avgP1Bingos
  printf "Avg P2 bingos:  %.2f\n" avgP2Bingos
  printf "Avg P1 exchg:   %.2f\n" avgP1Exchanges
  printf "Avg P2 exchg:   %.2f\n" avgP2Exchanges
  printf "Avg P1 passes:  %.2f\n" avgP1Passes
  printf "Avg P2 passes:  %.2f\n" avgP2Passes

  return finalResults

-- | Progress reporter thread
progressReporter :: Int -> TVar Int -> IO ()
progressReporter total completedVar = go 0
  where
    go lastReported = do
      threadDelay 100000  -- 100ms
      current <- atomically $ readTVar completedVar
      when (current /= lastReported) $ do
        let pct = 100 * fromIntegral current / fromIntegral total :: Double
        printf "\rProgress: %d/%d (%.1f%%)" current total pct
      if current < total
        then go current
        else return ()

-- | Worker thread that plays games
workerThread :: KWG -> LetterDistribution -> AutoplayConfig -> SharedState -> TVar Int -> IO ()
workerThread kwg ld AutoplayConfig{..} SharedState{..} _ = loop
  where
    loop = do
      -- Get next game number atomically
      mGameNum <- atomically $ do
        n <- readTVar nextGameNum
        if n >= autoplayNumGames
          then return Nothing
          else do
            writeTVar nextGameNum (n + 1)
            return (Just n)

      case mGameNum of
        Nothing -> return ()  -- No more games
        Just gameNum -> do
          -- Create deterministic RNG for this game
          let gen = mkStdGen (autoplaySeed + gameNum)

          -- Play the game (pass KLV and WMP for equity-based move selection)
          result <- playOneGame autoplayKLV autoplayWMP kwg ld gen

          -- Aggregate result
          atomically $ do
            modifyTVar' aggregateResults (combineResults (singleToResults result))
            modifyTVar' gamesCompleted (+1)

          -- Continue
          loop

-- | Play a single game and return the result
playOneGame :: Maybe KLV -> Maybe WMP -> KWG -> LetterDistribution -> StdGen -> IO SingleGameResult
playOneGame mKlv mWmp kwg ld gen0 = do
  let (game0, gen1) = newGame ld kwg gen0
  go game0 gen1 0 0 0 0 0 0 0 0 0 0 0
  where
    go !game !gen !turns !p1Score !p2Score !p1Tiles !p2Tiles !p1Bingos !p2Bingos !p1Exchanges !p2Exchanges !p1Passes !p2Passes
      | isGameOver game =
          let scores = finalScores game
              (s1, s2) = case scores of
                           [("Player 1", a), ("Player 2", b)] -> (a, b)
                           [("Player 2", b), ("Player 1", a)] -> (a, b)
                           _ -> (0, 0)
              winner | s1 > s2   = 1
                     | s2 > s1   = 2
                     | otherwise = 0
          in return $ SingleGameResult winner turns s1 s2 p1Tiles p2Tiles p1Bingos p2Bingos p1Exchanges p2Exchanges p1Passes p2Passes

      | otherwise = do
          let player = currentPlayer game
              rack = playerRack player
              board = gameBoard game
              bagCount = rackTotal (gameBag game)

              -- Generate the best move using shadow pruning, WMP existence checking, and leave values
              bestMove = generateBestMove defaultMoveGenConfig mKlv mWmp kwg ld board rack bagCount

              (game', gen') = makeMove game bestMove gen

              -- Track statistics
              isP1 = gameCurrentIdx game == 0
              tilesPlayed = case moveType bestMove of
                              TilePlacement -> length (filter (\ml -> unML ml /= 0) (moveTiles bestMove))
                              _ -> 0
              isBingo = tilesPlayed == 7
              isExchange = moveType bestMove == Exchange
              isPass = moveType bestMove == Pass

              newP1Tiles = if isP1 then p1Tiles + tilesPlayed else p1Tiles
              newP2Tiles = if not isP1 then p2Tiles + tilesPlayed else p2Tiles
              newP1Bingos = if isP1 && isBingo then p1Bingos + 1 else p1Bingos
              newP2Bingos = if not isP1 && isBingo then p2Bingos + 1 else p2Bingos
              newP1Exchanges = if isP1 && isExchange then p1Exchanges + 1 else p1Exchanges
              newP2Exchanges = if not isP1 && isExchange then p2Exchanges + 1 else p2Exchanges
              newP1Passes = if isP1 && isPass then p1Passes + 1 else p1Passes
              newP2Passes = if not isP1 && isPass then p2Passes + 1 else p2Passes
              newP1Score = if isP1 then p1Score + moveScore bestMove else p1Score
              newP2Score = if not isP1 then p2Score + moveScore bestMove else p2Score

          go game' gen' (turns + 1) newP1Score newP2Score newP1Tiles newP2Tiles newP1Bingos newP2Bingos newP1Exchanges newP2Exchanges newP1Passes newP2Passes

-- | Run move generation comparison between WMP and non-WMP modes
-- Plays games and reports any positions where the two methods choose different moves
runMoveGenComparison :: KWG -> LetterDistribution -> Maybe KLV -> WMP -> Int -> Int -> IO ()
runMoveGenComparison kwg ld mKlv wmp numGames seed = do
  putStrLn "Running move generation comparison (WMP vs non-WMP)..."
  putStrLn ""
  go 0 0 (mkStdGen seed)
  where
    go !gameNum !totalDiffs !gen
      | gameNum >= numGames = do
          putStrLn ""
          putStrLn $ "Comparison complete: " ++ show numGames ++ " games, " ++
                     show totalDiffs ++ " move differences found"
      | otherwise = do
          let (game0, gen') = newGame ld kwg gen
          diffs <- playComparisonGame mKlv wmp kwg ld game0 gameNum 0
          when (diffs > 0) $
            putStrLn $ "Game " ++ show gameNum ++ ": " ++ show diffs ++ " differences"
          go (gameNum + 1) (totalDiffs + diffs) gen'

-- | Play a single game comparing WMP and non-WMP move generation
playComparisonGame :: Maybe KLV -> WMP -> KWG -> LetterDistribution -> Game -> Int -> Int -> IO Int
playComparisonGame mKlv wmp kwg ld game gameNum turnNum
  | isGameOver game = return 0
  | otherwise = do
      let player = currentPlayer game
          rack = playerRack player
          board = gameBoard game
          bagCount = rackTotal (gameBag game)

          -- Generate move WITHOUT WMP
          moveNoWmp = generateBestMove defaultMoveGenConfig mKlv Nothing kwg ld board rack bagCount

          -- Generate move WITH WMP
          moveWithWmp = generateBestMove defaultMoveGenConfig mKlv (Just wmp) kwg ld board rack bagCount

          -- Compare moves
          isDifferent = moveScore moveNoWmp /= moveScore moveWithWmp ||
                        moveType moveNoWmp /= moveType moveWithWmp ||
                        moveTiles moveNoWmp /= moveTiles moveWithWmp

      diffCount <- if isDifferent
        then do
          putStrLn ""
          putStrLn $ "=== DIFFERENCE at game " ++ show gameNum ++ ", turn " ++ show turnNum ++ " ==="
          putStrLn $ "Rack: " ++ ldToString ld (rackToList rack)
          putStrLn $ "Board tiles played: " ++ show (getTilesPlayed board)
          putStrLn $ "Bag count: " ++ show bagCount
          putStrLn ""
          putStrLn $ "Non-WMP move: " ++ showMoveCompact ld moveNoWmp
          putStrLn $ "WMP move:     " ++ showMoveCompact ld moveWithWmp
          putStrLn $ "Score diff: " ++ show (moveScore moveNoWmp - moveScore moveWithWmp)
          return 1
        else return 0

      -- Continue game using the non-WMP move (the correct one)
      gen <- newStdGen
      let (game', _) = makeMove game moveNoWmp gen
      restDiffs <- playComparisonGame mKlv wmp kwg ld game' gameNum (turnNum + 1)
      return (diffCount + restDiffs)

-- | Show a move in compact format
showMoveCompact :: LetterDistribution -> Move -> String
showMoveCompact ld m = case moveType m of
  Pass -> "PASS (score=" ++ show (moveScore m) ++ ")"
  Exchange -> "EXCH " ++ ldToString ld (moveTiles m) ++ " (score=" ++ show (moveScore m) ++ ")"
  TilePlacement ->
    let pos = [toEnum (fromEnum 'A' + moveCol m)] ++ show (moveRow m + 1)
        dirStr = case moveDir m of { Horizontal -> ""; Vertical -> "v" }
    in pos ++ dirStr ++ " " ++ ldToString ld (moveTiles m) ++ " (score=" ++ show (moveScore m) ++ ")"

