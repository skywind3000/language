module Main where
    fuck :: (Integer, String) -> String
    fuck (x, y) = (show x) ++ y

    main = do
        let suck x = fuck (10, x)
        putStrLn (fuck (1, "hahahah"))
        putStrLn (suck "sdfsdf")


