module Main where
    
    -- divide :: Number num => (num, num) -> num
    divide (x, y) = x / y

    concatstr :: ([Char], [Char]) -> [Char]
    concatstr (x, y) = x ++ y

    half x = divide(x, 2)
    newline x = concatstr(x, "\n")

    main = do
        print (half 10)
        print (newline "sucker")
        

