module Main where
    qsort [] = []
    qsort (s:t) = 
        qsort([x | x <- t, x < s]) ++
        [s] ++
        [x | x <- t, x == s] ++
        qsort([x | x <- t, x > s])

    main = do
        print (qsort [3, 1, 4, 1, 5, 9, 2, 6])


