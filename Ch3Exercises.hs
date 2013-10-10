
assertValue actual expected
    | actual == expected = "Pass!"
    | otherwise          = "Bad times!"

-- Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function.

coolCounter (x:xs) = 1 + coolCounter xs
coolCounter     [] = 0

testCoolCounter = do
    putStrLn "Testing coolCounter..."
    putStrLn (assertValue 5 (coolCounter [1, 2, 3, 4, 5]))
    putStrLn (assertValue 0 (coolCounter []))


-- Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating point number.)

meanie :: [Double] -> Double
meanie xs = (sum xs) / (coolCounter xs)

testMeanie = do
    putStrLn "\nTesting meanie..."
    putStrLn (assertValue 1.0 (meanie [1]))
    putStrLn (assertValue 3.0 (meanie [1, 2, 3, 4, 5]))
    putStrLn (assertValue 18.5 (meanie [12, 25]))


-- Turn a list into a palindrome, i.e. it should read the same both backwards and forwards. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].

palindromify :: [a] -> [a]
palindromify xs = xs ++ reverse xs

testPalindromify = do
    putStrLn "\nTesting palindromify..."
    putStrLn (assertValue [1, 2, 3, 3, 2, 1] (palindromify [1, 2, 3]))


-- Write a function that determines whether its input list is a palindrome.

--isZeePalindrome :: (x:xs) -> Bool
--isZeePalindrome (x:xs)
--    | null (tail xs) = (x == head xs)
--    | x == tail xs   = isZeePalindrome take(((length xs) - 1) xs)
--    | otherwise      = False

main = do
    testCoolCounter
    testMeanie
    testPalindromify
