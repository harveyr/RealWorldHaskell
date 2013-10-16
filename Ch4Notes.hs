import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

main = do
    -- List functions
    putStrLn "[length]"
    print (length [1, 2, 3])

    putStrLn "\n[null]"
    print (null [])
    print (null [1])
    
    putStrLn "\n[head, last, init tail]"
    print (head [1, 2, 3])
    print (last [1, 2, 3])
    print (tail [1, 2, 3])
    print (init [1, 2, 3])

    putStrLn "\n[++]"
    print ([1] ++ [2, 3])

    putStrLn "\n[concat]"
    -- Removes one level of nesting
    print (concat [[1, 2, 3], [4, 5, 6]])
    print (concat [[[1, 2], [3]], [[4, 5], [6]]])

    putStrLn "\n[reverse]"
    print (reverse [1, 2, 3])
    print (reverse "blarney")

    putStrLn "\n[and, or (with list-of-bool argument)]"
    print (and [True, False, True])
    print (and [])
    print (or [False, False, True, False])
    print (or [])

    putStrLn "\n[all, any]"
    print (all odd [1, 3, 5])
    print (all odd [1, 3, 4])
    print (all odd [])  -- True
    print (any even [1, 3, 5])
    print (any even [1, 3, 4])
    print (any even []) -- False

    putStrLn "\n[take, drop]"
    print (take 3 "blarney")  -- "bla"
    print (take 2 [1]) -- [1]
    print (drop 3 "blarney")  -- "rney"
    -- error with: drop 1 [] (but text uses this as an example...)

    putStrLn "\n[splitAt]"
    print (splitAt 3 "blarney")  -- ("bla","rney")

    putStrLn "\n[takeWhile, dropWhile]"
    print (takeWhile odd [1, 3, 5, 6, 7, 8, 9])  -- [1,3,5]
    print (dropWhile even [2, 4, 6, 7, 8, 9])  -- [7,8,9]

    putStrLn "\n[span, break]"
    -- [Book] break consumes its input while its predicate fails, while span consumes while its predicate succeeds
    print (span odd [1, 3, 5, 6, 7, 8, 9])  -- ([1,3,5], [6,7,8,9])
    print (break odd [2, 4, 6, 7, 8, 9])  -- ([2,4,6], [7,8,9])

    putStrLn "\n[elem, notElem]"
    print (elem 3 [1, 2, 3])
    print(3 `elem` [1, 2, 3])  -- infix syntax
    print (notElem 4 [1, 2, 3])

    putStrLn "\n[filter]"
    print (filter odd [1, 2, 3, 4, 5])


    putStrLn "\n[Data.List: isPrefixOf, isSuffixOf, isInfixOf]"
    print ("blar" `isPrefixOf` "blarney")
    print ("sandwich" `isInfixOf` "there's a sandwich in my brain!")
    print ("brain!" `isSuffixOf` "there's a sandwich in my brain!")

    putStrLn "\n[zip, zipWith]"
    print (zip [1, 2, 3, 4, 5] "sugary")

    -- zipWith takes two lists and applies a function to each pair of elements, generating a list that is the same length as the shorter of the two
    print (zipWith (+) [1, 2, 3] [4, 5, 6])

    putStrLn "\n[lines, unlines, words, unwords]"
    print (lines "taco\nsalad")
    print (unlines ["ferret", "face"])

    print (words "\nland salmon \r ahead \t me matees\r\n\n(n stuff)")
    print (unwords ["practice", "makes", "better", "(sometimes)"])


