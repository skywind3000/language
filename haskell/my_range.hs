module Main where
    myRange start step = start:(myRange (start + step) step)

    main = do
        print (take 10 (myRange 10 1))
        print (take 5 (myRange 0 5))
        -- print (myRange 0 5)

