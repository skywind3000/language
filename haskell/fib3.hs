module Main where
    fibNextPair :: (Integer, Integer) -> (Integer, Integer)
    fibNextPair (x, y) = (y, x + y)

    fibNthPair :: Integer -> (Integer, Integer)
    fibNthPair 0 = (0, 1)
    fibNthPair 1 = (1, 1)
    fibNthPair n = fibNextPair(fibNthPair(n - 1))

    fib :: Integer -> Integer
    fib = fst . fibNthPair

    main = print(fib 6)


