module Main where

    colors = [0, 1, 2]
    possible = [(a, m, g, t, f) | 
        a<-colors, 
        m<-colors,
        g<-colors,
        t<-colors,
        f<-colors,
        m /= t,
        m /= a,
        a /= t,
        a /= m,
        a /= g,
        a /= f,
        g /= f,
        g /= t
        ]
    
    main = do
        putStrLn "maps"
        print possible


