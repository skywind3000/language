module Main where
    reverse2 (s, e) 
        | (length(s) == 0) = e
        | otherwise = reverse2(tail s, (head s):e)

    reverse3 s = (reverse2 (s, []))

    reverse4 ([], e) = e
    reverse4 (s:t, e) = reverse4(t, s:e)

    reverse5 s = reverse4(s, [])

    main = do
        print (reverse2 ([1,2,3,4], []))
        print (reverse3 [1,2,3,4])
        print (reverse5 [1,2,3,4])




