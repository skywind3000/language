module Main where
    factorial :: Integer -> Integer
    factorial x
        | x > 1 = factorial(x - 1) * x
        | otherwise = 1

    main = putStrLn "Fuck you"


