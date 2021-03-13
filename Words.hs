module Words (Byte(Byte),Word(Word), bit) where

import Prelude hiding (Word)
import Data.Bits

-- Byte is 8 Bits
data Byte = Byte
    Bool Bool Bool Bool
    Bool Bool Bool Bool
    deriving (Eq, Show)

readIntByte :: Int -> Byte
readIntByte n = Byte (bit 7) (bit 6) (bit 5) (bit 4) (bit 3) (bit 2) (bit 1) (bit 0)
    where bit i = (n .&. (shiftL 1 i)) /= 0

instance Read Byte where
    readsPrec _ s = [(readIntByte $ read s, "")]

getBit (Byte _ _ _ _ _ _ _ b) 0 = b
getBit (Byte _ _ _ _ _ _ b _) 1 = b
getBit (Byte _ _ _ _ _ b _ _) 2 = b
getBit (Byte _ _ _ _ b _ _ _) 3 = b
getBit (Byte _ _ _ b _ _ _ _) 4 = b
getBit (Byte _ _ b _ _ _ _ _) 5 = b
getBit (Byte _ b _ _ _ _ _ _) 6 = b
getBit (Byte b _ _ _ _ _ _ _) 7 = b


-- 32 Bit words (4 bytes)
-- Convention is big-endian although it doesn't really matter
data Word = Word Byte Byte Byte Byte
    deriving (Eq, Show)

-- Indexing
-- getBit (Word b3 b2 b1 b0) ind
--     | ind < 0   = error "invalid index"
--     | ind < 8   = bit b0 ind
--     | ind < 16  = bit b1 (mod ind 8)
--     | ind < 24  = bit b2 (mod ind 8)
--     | ind < 32  = bit b3 (mod ind 8)
--     | otherwise = error "invalid index"

readIntWord :: Int -> Word
readIntWord n = (Word
    (readIntByte (((shiftL 1 8) - 1) .&. n))
    (readIntByte ((shiftL ((shiftL 1 8) - 1) 8) .&. n))
    (readIntByte ((shiftL ((shiftL 1 8) - 1) 16) .&. n))
    (readIntByte ((shiftL ((shiftL 1 8) - 1) 24) .&. n)))

instance Read Word where
    readsPrec _ s = [(readIntWord $ read s, "")]