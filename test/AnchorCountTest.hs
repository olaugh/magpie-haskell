module Main where

import Magpie
import Magpie.Shadow
import Magpie.WMPMoveGen
import Magpie.WMP (loadWMP)

main :: IO ()
main = do
  kwg <- loadKWG "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/lexica/CSW21.kwg"
  wmp <- loadWMP "/Users/olaugh/sources/jan14-magpie/MAGPIE/data/lexica/CSW21.wmp"
  let ld = defaultEnglishLD
      rack = rackFromList (ldSize ld) $ case ldFromString ld "AEIOU" of Just mls -> mls; Nothing -> []
      board = standardBoard
      cfg = defaultShadowConfig { shadowBingoBonus = 50, shadowBagCount = 100 }
      
      -- Without WMP
      wmgNoWmp = wmpMoveGenInit Nothing rack
      anchorsNoWmp = generateAnchors cfg kwg ld board rack wmgNoWmp
      
      -- With WMP
      wmgWmp = wmpMoveGenCheckNonplaythroughExistence False (const (Equity 0)) $
               wmpMoveGenResetPlaythrough $
               wmpMoveGenInit (Just wmp) rack
      anchorsWmp = generateAnchors cfg kwg ld board rack wmgWmp
      
  putStrLn $ "Without WMP: " ++ show (length anchorsNoWmp) ++ " anchors"
  putStrLn $ "With WMP:    " ++ show (length anchorsWmp) ++ " anchors"
