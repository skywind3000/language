module Main where
    number = [1..9]
    mtable = [(x, y, x * y) | x <- number, y <-number, x < y]

    main = do
        print mtable

