initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
        where (f:_) = firstname
              (l:_) = lastname



