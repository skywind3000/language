module Main where

    valid :: Char -> Bool
    valid x = (x >= '0' && x <= '9')

    indexof ([], x, n) = -1
    indexof (s:t, x, n) 
        | (x == s) = n
        | otherwise = indexof(t, x, n + 1)

    index (list, x) = indexof(list, x, 0)

    char2nr :: Char -> Integer
    char2nr x = index("0123456789", x)

    list_reverse ([], e) = e
    list_reverse (s:t, e) = list_reverse(t, s:e)

    reverse_list s = list_reverse(s, [])

    int_reverse :: [Char] -> Integer
    int_reverse [] = 0
    int_reverse (s:t) = char2nr(s) + 10 * int_reverse(t)

    int_parse list = int_reverse(reverse_list list)

    fraction_imp :: Floating float => [Char] -> float
    fraction_imp [] = 0.0
    fraction_imp (s:t) = fromIntegral(char2nr(s)) + fraction_imp(t) * 0.1

    fraction_parse n = 0.1 * fraction_imp(n)

    drop_while ([], n) = []
    drop_while (s:t, n) 
        | s == n = t
        | otherwise = drop_while(t, n)

    take_until ([], n) = []
    take_until (s:t, n)
        | s == n = []
        | otherwise = s:take_until(t, n)

    integer_part n = int_parse [x | x <- n, valid x]
    fraction_part n = fraction_parse [x | x <- n, valid x]

    string2num n = 
        fromIntegral(integer_part(take_until(n, '.'))) + 
        fraction_part(drop_while(n, '.'))

    main = do
        print (valid '9')
        print (valid 'a')
        print (index ([1,2,3,4,5], 4))
        print (index ([1,2,3,4,5], 9))
        print (char2nr '9')
        print (reverse_list "Hello, World")
        print (int_parse "1234")
        print (fraction_parse "1234")
        print (take_until ("13.14159", '.'))
        print (drop_while ("13.14159", '.'))
        print (string2num "3.1415926" )
        print (string2num "$2,345,678.222" )


