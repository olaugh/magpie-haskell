-- | KWG (Kurnia Word Graph) format reader
-- Based on the format from https://github.com/andy-k/wolges
-- and the MAGPIE implementation

module KWG
  ( KWG
  , loadKWG
  , dawgRoot
  , gaddagRoot
  , nodeArcIndex
  , nodeTile
  , nodeIsEnd
  , nodeAccepts
  , getNode
  , numNodes
  ) where

import qualified Data.ByteString as BS
import Data.Word (Word32, Word8)
import Data.Bits ((.&.), shiftR, testBit)
import qualified Data.Vector.Unboxed as V

-- | A KWG is just a vector of 32-bit nodes
newtype KWG = KWG (V.Vector Word32)
  deriving (Show)

-- Constants from kwg_defs.h
nodeIsEndFlag :: Word32
nodeIsEndFlag = 0x400000

nodeAcceptsFlag :: Word32
nodeAcceptsFlag = 0x800000

arcIndexMask :: Word32
arcIndexMask = 0x3FFFFF

tileBitOffset :: Int
tileBitOffset = 24

-- | Load a KWG file (little-endian 32-bit words)
loadKWG :: FilePath -> IO KWG
loadKWG path = do
  bs <- BS.readFile path
  let words = bytesToWord32s (BS.unpack bs)
  return $ KWG (V.fromList words)

-- | Convert bytes to little-endian Word32s
bytesToWord32s :: [Word8] -> [Word32]
bytesToWord32s [] = []
bytesToWord32s (a:b:c:d:rest) =
  let w = fromIntegral a
        + fromIntegral b * 256
        + fromIntegral c * 65536
        + fromIntegral d * 16777216
  in w : bytesToWord32s rest
bytesToWord32s _ = [] -- Ignore trailing bytes

-- | Get a node by index
getNode :: KWG -> Word32 -> Word32
getNode (KWG nodes) idx = nodes V.! fromIntegral idx

-- | Number of nodes
numNodes :: KWG -> Int
numNodes (KWG nodes) = V.length nodes

-- | Get the DAWG root node index (from node 0)
dawgRoot :: KWG -> Word32
dawgRoot kwg = nodeArcIndex (getNode kwg 0)

-- | Get the GADDAG root node index (from node 1)
gaddagRoot :: KWG -> Word32
gaddagRoot kwg = nodeArcIndex (getNode kwg 1)

-- | Extract the arc index (pointer to children) from a node
nodeArcIndex :: Word32 -> Word32
nodeArcIndex node = node .&. arcIndexMask

-- | Extract the tile/letter from a node (0=blank, 1=A, 2=B, etc.)
nodeTile :: Word32 -> Word32
nodeTile node = node `shiftR` tileBitOffset

-- | Check if this is the last sibling in a node set
nodeIsEnd :: Word32 -> Bool
nodeIsEnd node = testBit node 22

-- | Check if this node accepts (completes a valid word)
nodeAccepts :: Word32 -> Bool
nodeAccepts node = testBit node 23
