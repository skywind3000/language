module Main where
    allEven :: [Integer] -> [Integer]
    allEven [] = []
    allEven (h:t) = if even h then h:allEven(t) else allEven(t)

    main = print(allEven [1,2,3,4,5,6,7])


