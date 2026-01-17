-- | Word Anagrammer REPL using KWG dictionary

module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
import Control.Monad (unless)

import KWG
import Anagram

main :: IO ()
main = do
  args <- getArgs
  case args of
    [kwgPath] -> do
      putStrLn $ "Loading dictionary: " ++ kwgPath
      kwg <- loadKWG kwgPath
      putStrLn $ "Loaded " ++ show (numNodes kwg) ++ " nodes"
      putStrLn ""
      putStrLn "Enter letters to find anagrams (or 'quit' to exit)"
      putStrLn "Use ? or _ for blank tiles"
      putStrLn "Prefix with + to find sub-anagrams (words using some letters)"
      putStrLn ""
      hSetBuffering stdout LineBuffering
      repl kwg
    _ -> do
      putStrLn "Usage: anagram <dictionary.kwg>"
      putStrLn ""
      putStrLn "Example: anagram CSW21.kwg"

repl :: KWG -> IO ()
repl kwg = do
  putStr "> "
  hFlush stdout
  input <- getLine
  unless (input == "quit" || input == "exit" || input == "q") $ do
    processInput kwg input
    repl kwg

processInput :: KWG -> String -> IO ()
processInput kwg input
  | null input = return ()
  | head input == '+' = do
      -- Sub-anagram mode: find words using some or all letters
      let letters = tail input
      let results = findSubAnagrams kwg letters
      printResults results
  | otherwise = do
      -- Exact anagram mode: find words using all letters
      let results = findAnagrams kwg input
      printResults results

printResults :: [String] -> IO ()
printResults [] = putStrLn "(no results)"
printResults results = do
  putStrLn $ show (length results) ++ " result(s):"
  mapM_ putStrLn results
  putStrLn ""
