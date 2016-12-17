module Main where
    every2 x = x : every2 (x + 2)
    every4 x = x : every4 (x + 4)

    combo (x, y) = [(a + b + c) | (a, b, c) <- (zip3 (every2 x) (every4 y) [0..])]

    main = do
        print (take 5 (every2 0))
        print (take 5 (every4 0))
        print (take 10 (combo(3, 1)))


