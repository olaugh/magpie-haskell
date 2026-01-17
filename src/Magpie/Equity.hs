{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Equity type for Scrabble move evaluation
--
-- Equity is represented as a fixed-point integer with 1000x resolution.
-- This matches MAGPIE's representation for efficient storage and computation.
--
-- Example: an equity of 12.345 is stored as 12345
module Magpie.Equity
  ( Equity(..)
  , equityResolution
  , equityUndefined
  , equityInitial
  , equityPass
  , equityMinValue
  , equityMaxValue

    -- * Conversions
  , doubleToEquity
  , equityToDouble
  , intToEquity
  , equityToInt

    -- * Operations
  , equityNegate
  , equityIsInteger
  ) where

import Data.Int (Int32)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

-- | Equity is a fixed-point integer with 1000x resolution
newtype Equity = Equity { unEquity :: Int32 }
  deriving (Eq, Ord, Show, Num)

-- Unbox instance for Equity (delegate to Int32)
newtype instance VU.MVector s Equity = MV_Equity (VU.MVector s Int32)
newtype instance VU.Vector Equity = V_Equity (VU.Vector Int32)

instance VGM.MVector VU.MVector Equity where
  basicLength (MV_Equity v) = VGM.basicLength v
  basicUnsafeSlice i n (MV_Equity v) = MV_Equity $ VGM.basicUnsafeSlice i n v
  basicOverlaps (MV_Equity v1) (MV_Equity v2) = VGM.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Equity <$> VGM.basicUnsafeNew n
  basicInitialize (MV_Equity v) = VGM.basicInitialize v
  basicUnsafeReplicate n (Equity x) = MV_Equity <$> VGM.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Equity v) i = Equity <$> VGM.basicUnsafeRead v i
  basicUnsafeWrite (MV_Equity v) i (Equity x) = VGM.basicUnsafeWrite v i x
  basicClear (MV_Equity v) = VGM.basicClear v
  basicSet (MV_Equity v) (Equity x) = VGM.basicSet v x
  basicUnsafeCopy (MV_Equity v1) (MV_Equity v2) = VGM.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Equity v1) (MV_Equity v2) = VGM.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Equity v) n = MV_Equity <$> VGM.basicUnsafeGrow v n

instance VG.Vector VU.Vector Equity where
  basicUnsafeFreeze (MV_Equity v) = V_Equity <$> VG.basicUnsafeFreeze v
  basicUnsafeThaw (V_Equity v) = MV_Equity <$> VG.basicUnsafeThaw v
  basicLength (V_Equity v) = VG.basicLength v
  basicUnsafeSlice i n (V_Equity v) = V_Equity $ VG.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Equity v) i = Equity <$> VG.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Equity mv) (V_Equity v) = VG.basicUnsafeCopy mv v
  elemseq _ (Equity x) z = VG.elemseq (undefined :: VU.Vector Int32) x z

instance VU.Unbox Equity

-- | Resolution: 1000 (so 1.0 = 1000, 0.001 = 1)
equityResolution :: Int32
equityResolution = 1000

-- | Undefined equity (sentinel value)
equityUndefined :: Equity
equityUndefined = Equity minBound

-- | Initial equity (before evaluation)
equityInitial :: Equity
equityInitial = Equity (minBound + 1)

-- | Pass equity (special marker for pass moves)
equityPass :: Equity
equityPass = Equity (minBound + 2)

-- | Minimum valid equity value
equityMinValue :: Equity
equityMinValue = Equity (minBound + 3)

-- | Maximum valid equity value (symmetric with min)
equityMaxValue :: Equity
equityMaxValue = Equity (-(minBound + 3))

-- | Convert a Double to Equity
doubleToEquity :: Double -> Equity
doubleToEquity x = Equity $ fromIntegral rounded
  where
    rounded = round (x * fromIntegral equityResolution) :: Int

-- | Convert Equity to Double
equityToDouble :: Equity -> Double
equityToDouble (Equity eq) = fromIntegral eq / fromIntegral equityResolution

-- | Convert an Int to Equity (multiplies by resolution)
intToEquity :: Int -> Equity
intToEquity x = Equity $ fromIntegral x * equityResolution

-- | Convert Equity to Int (divides by resolution, must be integer equity)
equityToInt :: Equity -> Int
equityToInt (Equity eq) = fromIntegral (eq `div` equityResolution)

-- | Negate an equity value
equityNegate :: Equity -> Equity
equityNegate (Equity eq) = Equity (-eq)

-- | Check if equity is an integer (divisible by resolution)
equityIsInteger :: Equity -> Bool
equityIsInteger (Equity eq) = eq `mod` equityResolution == 0
