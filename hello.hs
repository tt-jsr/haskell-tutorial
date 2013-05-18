
main = do   
    do putStrLn "Enter name: "
    line <- getLine  
    let t = checkname line
    putStrLn t

checkname :: String -> String
checkname s 
    | s == "jeff" = "Haskell is great"
    | s == "simon" = "Haskell is ok"
    | s == "greg" = "Haskell sucks"
    | otherwise = "I'm not sure"
  
