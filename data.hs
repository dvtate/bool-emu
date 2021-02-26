module Data (Byte bit Word byte) where

import Prelude
import Data.Bits

-- Byte is 8 Bits
data Byte = Byte
    Bool Bool Bool Bool
    Bool Bool Bool Bool
    deriving (Eq, Show, Read)

readInt :: Int -> Byte
readInt n = Byte (bit 7) (bit 6) (bit 5) (bit 4) (bit 3) (bit 2) (bit 1) (bit 0)
    where bit i = (n .&. (shiftL 1 i)) /= 0

instance Read Byte where
    readsPrec _ s = [(readInt $ read s, "")]

bit (Byte _ _ _ _ _ _ _ b) 0 = b
bit (Byte _ _ _ _ _ _ b _) 1 = b
bit (Byte _ _ _ _ _ b _ _) 2 = b
bit (Byte _ _ _ _ b _ _ _) 3 = b
bit (Byte _ _ _ b _ _ _ _) 4 = b
bit (Byte _ _ b _ _ _ _ _) 5 = b
bit (Byte _ b _ _ _ _ _ _) 6 = b
bit (Byte b _ _ _ _ _ _ _) 7 = b



-- 32 Bit words (4 bytes)
-- Convention is big-endian although it doesn't really matter
data Word = Word Byte Byte Byte Byte
    deriving (Eq, Show)

-- Indexing
byte (Word b3 b2 b1 b0) ind
    | ind < 0   = error "invalid index"
    | ind < 8   = bit b0 ind
    | ind < 16  = bit b1 (mod ind 8)
    | ind < 24  = bit b2 (mod ind 8)
    | ind < 32  = bit b3 (mod ind 8)
    | otherwise = error "invalid index"
