{-# LANGUAGE ScopedTypeVariables #-}

-- | Magpie REPL - Interactive Scrabble engine
module Main where

import Magpie
import Magpie.Autoplay
import Magpie.KLV (KLV, loadKLV)

import System.Environment (getArgs)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
import System.Random (getStdGen, StdGen, newStdGen)
import Control.Monad (unless)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Char (toUpper, isDigit)
import Text.Read (readMaybe)
import Control.Exception (catch, SomeException)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [kwgPath] -> runWithKWG kwgPath Nothing
    [kwgPath, ldPath] -> runWithKWG kwgPath (Just ldPath)
    _ -> do
      putStrLn "Magpie - Scrabble Engine"
      putStrLn ""
      putStrLn "Usage: magpie <dictionary.kwg> [letter_distribution.csv]"
      putStrLn ""
      putStrLn "Example: magpie CSW21.kwg english.csv"
      putStrLn ""
      putStrLn "Commands:"
      putStrLn "  gen <rack>     - Generate moves for a rack"
      putStrLn "  play           - Start an interactive game"
      putStrLn "  autoplay [n]   - Auto-play n games (default 1)"
      putStrLn "  anagram <rack> - Find anagrams"
      putStrLn "  quit           - Exit"

runWithKWG :: FilePath -> Maybe FilePath -> IO ()
runWithKWG kwgPath mldPath = do
  putStrLn $ "Loading dictionary: " ++ kwgPath
  kwg <- loadKWG kwgPath

  ld <- case mldPath of
    Just ldPath -> do
      putStrLn $ "Loading letter distribution: " ++ ldPath
      loadLetterDistribution ldPath
    Nothing -> do
      putStrLn "Using default English letter distribution"
      return defaultEnglishLD

  -- Load KLV file at startup (same base name as KWG with .klv2 extension)
  let klvPath = replaceExtension kwgPath "klv2"
  mKlv <- tryLoadKLV klvPath

  putStrLn $ "Loaded " ++ show (numNodes kwg) ++ " nodes"
  putStrLn ""
  putStrLn "Commands: gen <rack>, play, autoplay [n] [-t threads] [-s seed], anagram <rack>, quit"
  putStrLn ""

  hSetBuffering stdout LineBuffering
  repl kwgPath kwg ld mKlv

repl :: FilePath -> KWG -> LetterDistribution -> Maybe KLV -> IO ()
repl kwgPath kwg ld mKlv = do
  putStr "magpie> "
  hFlush stdout
  input <- getLine
  let ws = words input
  case ws of
    ["quit"] -> putStrLn "Goodbye!"
    ["q"] -> putStrLn "Goodbye!"
    ["exit"] -> putStrLn "Goodbye!"
    ("gen":rack:_) -> do
      generateForRack kwg ld (map toUpper rack)
      repl kwgPath kwg ld mKlv
    ("anagram":rack:_) -> do
      findAnagramsCmd kwg ld (map toUpper rack)
      repl kwgPath kwg ld mKlv
    ["play"] -> do
      playGame kwg ld
      repl kwgPath kwg ld mKlv
    ("autoplay":args') -> do
      let config = parseAutoplayArgs mKlv args'
      _ <- runAutoplayThreaded kwg ld config
      repl kwgPath kwg ld mKlv
    [] -> repl kwgPath kwg ld mKlv
    _ -> do
      putStrLn "Unknown command. Try: gen <rack>, play, anagram <rack>, quit"
      repl kwgPath kwg ld mKlv

-- | Generate moves for a rack on an empty board
generateForRack :: KWG -> LetterDistribution -> String -> IO ()
generateForRack kwg ld rackStr = do
  case ldFromString ld rackStr of
    Nothing -> putStrLn $ "Invalid rack: " ++ rackStr
    Just mls -> do
      let rack = rackFromList (ldSize ld) mls
          board = standardBoard
          moves = take 20 $ generateMoves kwg ld board rack

      if null moves
        then putStrLn "No moves found."
        else do
          putStrLn $ "Top " ++ show (length moves) ++ " moves:"
          mapM_ (printMove ld) moves

printMove :: LetterDistribution -> Move -> IO ()
printMove ld move =
  case moveType move of
    Pass -> putStrLn "  PASS"
    Exchange -> putStrLn $ "  EXCH " ++ ldToString ld (moveTiles move)
    TilePlacement -> do
      let pos = [toEnum (fromEnum 'A' + moveCol move)] ++ show (moveRow move + 1)
          dirStr = case moveDir move of
            Horizontal -> ""
            Vertical -> " (v)"
          tiles = ldToString ld (moveTiles move)
      putStrLn $ "  " ++ pos ++ dirStr ++ " " ++ tiles ++ " (" ++ show (moveScore move) ++ ")"

-- | Parse autoplay arguments: [n] [-t threads] [-s seed] [-v] [--no-klv]
-- Uses the pre-loaded KLV unless --no-klv is specified
parseAutoplayArgs :: Maybe KLV -> [String] -> AutoplayConfig
parseAutoplayArgs mKlv args =
  let (config, useKlv) = parseArgs defaultAutoplayConfig True args
  in if useKlv
     then config { autoplayKLV = mKlv }
     else config
  where
    parseArgs config useKlv [] = (config, useKlv)
    parseArgs config useKlv ("-t":tStr:rest) =
      case readMaybe tStr of
        Just t -> parseArgs (config { autoplayNumThreads = t }) useKlv rest
        Nothing -> parseArgs config useKlv rest
    parseArgs config useKlv ("-s":sStr:rest) =
      case readMaybe sStr of
        Just s -> parseArgs (config { autoplaySeed = s }) useKlv rest
        Nothing -> parseArgs config useKlv rest
    parseArgs config useKlv ("-v":rest) = parseArgs (config { autoplayVerbose = True }) useKlv rest
    parseArgs config _ ("--no-klv":rest) = parseArgs config False rest
    parseArgs config useKlv (nStr:rest) =
      case readMaybe nStr of
        Just n -> parseArgs (config { autoplayNumGames = n }) useKlv rest
        Nothing -> parseArgs config useKlv rest

-- | Replace file extension
replaceExtension :: FilePath -> String -> FilePath
replaceExtension path ext =
  let base = reverse $ drop 1 $ dropWhile (/= '.') $ reverse path
  in if null base then path ++ "." ++ ext else base ++ "." ++ ext

-- | Try to load a KLV file, returning Nothing on failure
tryLoadKLV :: FilePath -> IO (Maybe KLV)
tryLoadKLV path = do
  result <- (Just <$> loadKLV path) `catch` (\(_ :: SomeException) -> return Nothing)
  case result of
    Just klv -> do
      putStrLn $ "Loaded leave values: " ++ path
      return (Just klv)
    Nothing -> do
      putStrLn "Note: No KLV file found, using score-only move selection"
      return Nothing

-- | Find anagrams command
findAnagramsCmd :: KWG -> LetterDistribution -> String -> IO ()
findAnagramsCmd kwg ld rackStr = do
  case ldFromString ld rackStr of
    Nothing -> putStrLn $ "Invalid rack: " ++ rackStr
    Just mls -> do
      let rack = rackFromList (ldSize ld) mls
          results = findSubAnagrams kwg ld rack

      if null results
        then putStrLn "No anagrams found."
        else do
          putStrLn $ show (length results) ++ " word(s) found:"
          mapM_ putStrLn results

-- | Find sub-anagrams using the DAWG
findSubAnagrams :: KWG -> LetterDistribution -> Rack -> [String]
findSubAnagrams kwg ld rack = search (dawgRoot kwg) rack []
  where
    search nodeIdx r prefix
      | nodeIdx == 0 = []
      | otherwise = searchSiblings nodeIdx r prefix

    searchSiblings nodeIdx r prefix =
      let node = getNode kwg nodeIdx
          tile = nodeTile node
          ml = MachineLetter (fromIntegral tile)
          arcIdx = nodeArcIndex node
          isEnd = nodeIsEnd node
          accepts = nodeAccepts node

          -- Try using this tile from rack
          results = tryTile r ml arcIdx accepts prefix

          -- Try using blank if we have one
          blankResults = if tile /= 0 && rackHasLetter (MachineLetter 0) r
                         then tryBlank r ml arcIdx accepts prefix
                         else []

          -- Continue to next sibling
          siblingResults = if isEnd then [] else searchSiblings (nodeIdx + 1) r prefix

      in results ++ blankResults ++ siblingResults

    tryTile r ml arcIdx accepts prefix
      | not (rackHasLetter ml r) = []
      | otherwise =
          let newR = rackTakeLetter ml r
              newPrefix = prefix ++ [ldToChar ld ml]
              foundWords = if accepts then [newPrefix] else []
              childWords = if arcIdx /= 0 then search arcIdx newR newPrefix else []
          in foundWords ++ childWords

    tryBlank r ml arcIdx accepts prefix =
      let newR = rackTakeLetter (MachineLetter 0) r
          newPrefix = prefix ++ [ldToChar ld ml]
          foundWords = if accepts then [newPrefix] else []
          childWords = if arcIdx /= 0 then search arcIdx newR newPrefix else []
      in foundWords ++ childWords

-- | Play an interactive game
playGame :: KWG -> LetterDistribution -> IO ()
playGame kwg ld = do
  putStrLn "\nStarting new game..."
  gen <- getStdGen
  let (game, gen') = newGame ld kwg gen
  gameLoop game gen'

gameLoop :: Game -> StdGen -> IO ()
gameLoop game gen
  | isGameOver game = do
      putStrLn "\nGame Over!"
      putStrLn "Final scores:"
      mapM_ (\(name, score) -> putStrLn $ "  " ++ name ++ ": " ++ show score)
            (finalScores game)
  | otherwise = do
      putStrLn $ showGame game

      let player = currentPlayer game
          rack = playerRack player

      putStrLn $ "\n" ++ playerName player ++ "'s turn"
      putStrLn "Enter: move (e.g., 'H8 WORD'), 'hint', 'pass', 'exchange ABC', or 'quit'"

      putStr "> "
      hFlush stdout
      input <- getLine

      case words input of
        ["quit"] -> putStrLn "Game abandoned."
        ["pass"] -> do
          let move = Move Pass 0 0 Horizontal [] 0 0 0
              (game', gen') = makeMove game move gen
          gameLoop game' gen'
        ["hint"] -> do
          let moves = take 10 $ generateMoves (gameKWG game) (gameLD game) (gameBoard game) rack
          putStrLn "Top moves:"
          mapM_ (printMove (gameLD game)) moves
          gameLoop game gen
        ("exchange":tiles:_) -> do
          let gld = gameLD game
          case ldFromString gld tiles of
            Nothing -> do
              putStrLn "Invalid tiles"
              gameLoop game gen
            Just mls -> do
              let move = Move Exchange 0 0 Horizontal mls (length mls) 0 0
                  (game', gen') = makeMove game move gen
              gameLoop game' gen'
        (posStr:wordStr:_) -> do
          let gld = gameLD game
          case parsePosition posStr of
            Nothing -> do
              putStrLn "Invalid position (e.g., H8 or 8H)"
              gameLoop game gen
            Just (row, col, dir) -> do
              case ldFromString gld wordStr of
                Nothing -> do
                  putStrLn "Invalid word"
                  gameLoop game gen
                Just mls -> do
                  let move = Move TilePlacement row col dir mls (length mls) 0 0
                      -- Recalculate score
                      score = scoreMove defaultMoveGenConfig gld (gameBoard game) move
                      move' = move { moveScore = score }
                      (game', gen') = makeMove game move' gen
                  gameLoop game' gen'
        _ -> do
          putStrLn "Invalid input"
          gameLoop game gen

-- | Parse a position like "H8" or "8H" or "H8v"
parsePosition :: String -> Maybe (Row, Col, Direction)
parsePosition s = case s of
  (c:rest)
    | c >= 'A' && c <= 'O' ->
        let col = fromEnum c - fromEnum 'A'
            (rowStr, dirStr) = span (`elem` ['0'..'9']) rest
        in case reads rowStr of
             [(row, _)] ->
               let dir = if "v" `elem` [dirStr] || "V" `elem` [dirStr]
                         then Vertical
                         else Horizontal
               in Just (row - 1, col, dir)
             _ -> Nothing
    | c >= '0' && c <= '9' ->
        let (rowStr, rest') = span (`elem` ['0'..'9']) s
        in case rest' of
             (colC:dirStr)
               | colC >= 'A' && colC <= 'O' ->
                   case reads rowStr of
                     [(row, _)] ->
                       let col = fromEnum colC - fromEnum 'A'
                           dir = if "v" `elem` [dirStr] || "V" `elem` [dirStr]
                                 then Vertical
                                 else Horizontal
                       in Just (row - 1, col, dir)
                     _ -> Nothing
             _ -> Nothing
  _ -> Nothing
