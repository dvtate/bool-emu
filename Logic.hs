module Logic(and,or,xor,not,nor,nand) where

import qualified Prelude

-- here we're re-defining some things for seemingly no reason

and = (Prelude.&&)
or = (Prelude.||)

xor :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
xor = (Prelude./=)
not = Prelude.not
nor a b = not (or a b)
nand a b = not (and a b)