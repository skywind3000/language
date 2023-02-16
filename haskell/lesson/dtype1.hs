removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c|c<-st, c `elem` ['A'..'Z']]

main =
    print(removeNonUppercase "Hello, World !!")

