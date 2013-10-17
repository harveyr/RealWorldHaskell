foldlSum xs = foldl step 0 xs
    where step acc x = acc + x

simplerFoldlSum :: [Integer] -> Integer
simplerFoldlSum xs = foldl (+) 0 xs


foldrFilter p xs = foldr step [] xs
    where step x ys | p x       = x:ys
                    | otherwise = ys


foldrAppend xs ys = foldr (:) ys xs


main = do
    print(foldlSum [1, 2, 3])
    print(simplerFoldlSum [1, 2, 3])

    print(foldrFilter odd [1, 2, 3, 4, 5])

    print(foldrAppend [1, 2, 3] [4, 5, 6])
