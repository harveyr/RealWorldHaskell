import Data.Char (digitToInt)
import Data.List (foldl')

-- Exercise 1. Use a fold to rewrite and improve upon the asInt function from the section called “Explicit recursion”.
foldToInt :: String -> Int
foldToInt xs = if head xs == '-'
    then negate (foldToInt (tail xs))
    else foldl' step 0 xs
        where step acc x = acc * 10 + digitToInt(x)

exercise1 = do
    putStrLn "\nExercise 1"
    print(foldToInt("101") == 101)
    print(foldToInt("-31337") == -31337)
    print(foldToInt("1798") == 1798)

main = do
    exercise1
