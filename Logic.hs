module Logic(and,or,xor,not,nor,nand) where

import qualified Prelude

-- here we're re-defining some things for no reason
-- but i think having them defined here could be useful eventually
-- and makes it clear that we're only allowed to use logic gates
-- as if we were making actual hardware (well maybe not quite but still)

-- TODO also redefine True and False as they would be replaced with +5v and GND

and = (Prelude.&&)
or = (Prelude.||)

xor :: Prelude.Bool -> Prelude.Bool -> Prelude.Bool
xor = (Prelude./=)
not = Prelude.not
nor a b = not (or a b)
nand a b = not (and a b)