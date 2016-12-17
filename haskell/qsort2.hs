module Main where
    qsort ([], cmp) = []
    qsort (s:t, cmp) = 
        qsort([x | x <- t, cmp(x, s) < 0], cmp) ++
        [s] ++
        [x | x <- t, cmp(x, s) == 0] ++
        qsort([x | x <- t, cmp(x, s) > 0], cmp)

    mycmp (x, y) 
        | x < y = -1
        | x == y = 0
        | x > y = 1

    main = do
        print (qsort ([3, 1, 4, 1, 5, 9, 2, 6], mycmp))
        print 12345


