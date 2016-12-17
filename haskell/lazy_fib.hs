module Main where
    lazyFib x y = x:(lazyFib y (x + y))
    fib = lazyFib 1 1

    fibNth x = head (drop (x - 1) (take (x) fib))

    main = do
        print (take 5 (lazyFib 0 1))
        print (take 5 (fib))
        print (take 5 (drop 20 (lazyFib 0 1)))
        print (fibNth 3)
        print (fibNth 6)
        
