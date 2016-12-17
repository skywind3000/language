module Main where
    generate n = [(x, y) | x <- n, y <- n, x < y]

    main = do
        print (generate ["black", "yellow", "blue", "white", "red"])

