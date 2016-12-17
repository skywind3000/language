module Main where
    even1 [] = []
    even1 (h:t) = if (even h) then (h:even1(t)) else even1(t)

    even2 x = [n | n <- x, even n]

    main = do
        print (even1 [1,2,3,4,5])
        print (even2 [1,2,3,4,5])

