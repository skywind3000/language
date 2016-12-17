module Main where
    squareAll list = map square list
        where square x = x * x

    main = do
        print (squareAll [1,2,3,4])

