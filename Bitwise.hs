module BitWise where

import qualified Logic
import qualified Words

class Shiftable a where
    shiftL :: a -> a -> a

instance Shiftable Words.Byte
    shiftL (Byte )