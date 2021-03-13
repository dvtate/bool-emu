module Math (add, Addable) where

import qualified Words
import qualified Logic


-- Sum with a carry bit
data Sum a = Sum a Bool
    deriving (Eq, Show)

-- Half adder
halfAdd :: Bool -> Bool -> Sum Bool
halfAdd a b = Sum (Logic.xor a b) (Logic.and a b)

-- Full adder
fullAdd :: Bool -> Bool -> Bool -> Sum Bool
fullAdd c a b = let partSum = Logic.xor a b
    in Sum (Logic.xor partSum c) (Logic.or (Logic.and partSum c) (Logic.and a b))



--
class Addable a where
    add :: Bool -> a -> a -> Sum a

instance Addable Words.Byte where
    add c (Words.Byte a7 a6 a5 a4 a3 a2 a1 a0) (Words.Byte b7 b6 b5 b4 b3 b2 b1 b0) = let
        Sum s0 c0 = fullAdd c a0 b0
        Sum s1 c1 = fullAdd c0 a1 b1
        Sum s2 c2 = fullAdd c1 a2 b2
        Sum s3 c3 = fullAdd c2 a3 b3
        Sum s4 c4 = fullAdd c3 a4 b4
        Sum s5 c5 = fullAdd c4 a5 b5
        Sum s6 c6 = fullAdd c5 a6 b6
        Sum s7 c7 = fullAdd c6 a7 b7
        in Sum (Words.Byte s7 s6 s5 s4 s3 s2 s1 s0) c7

instance Addable Words.Word where
    add c (Words.Word a3 a2 a1 a0) (Words.Word b3 b2 b1 b0) = let
        Sum s0 c0 = add c a0 b0
        Sum s1 c1 = add c0 a1 b1
        Sum s2 c2 = add c1 a2 b2
        Sum s3 c3 = add c2 a3 b3
        in Sum (Words.Word s3 s2 s1 s0) c3
