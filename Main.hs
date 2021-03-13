import qualified Words
import qualified Math
import Prelude
import IO


main :: IO ()
main = do
    putStrLn "enter a number"
    a <- getLine
    putStrLn "enter a number"
    b <- getLine
    (putStrLn . show . Math.add) (read a)::Words.Word (read b)::Words.Word
