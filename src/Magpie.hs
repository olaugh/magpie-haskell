-- | Magpie: A Scrabble engine in Haskell
-- Ported from the C implementation at github.com/domino14/magpie
module Magpie
  ( -- * Core Types
    module Magpie.Types

    -- * Letter Distribution
  , module Magpie.LetterDistribution

    -- * KWG (Dictionary)
  , module Magpie.KWG

    -- * Board
  , module Magpie.Board

    -- * Move Generation
  , module Magpie.MoveGen

    -- * Game
  , module Magpie.Game
  ) where

import Magpie.Types
import Magpie.LetterDistribution
import Magpie.KWG
import Magpie.Board
import Magpie.MoveGen
import Magpie.Game
