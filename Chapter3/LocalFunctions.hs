pluralise :: String -> [int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word 
          plural n = n ++ " " ++ word ++ "s"

