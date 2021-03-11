import Words
import Math
import Prelude
import IO


main :: IO ()
main = do
    putStrLn "enter a number"
    a <- getLine
    putStrLn "enter a number"
    b <- getLine
    (putStrLn . show . add) (read a)::Word (read b)::Word
