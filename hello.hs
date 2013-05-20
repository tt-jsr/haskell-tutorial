import System.IO
import Data.Char(toUpper)

main = do   
    putStrLn "Enter name: "
    line <- getLine  
    let t = checkname line
    putStrLn t

    inh <- openFile "input.txt" ReadMode
    outh <- openFile "output.txt" WriteMode
    mainloop inh outh
    hClose inh
    hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh
       if ineof
       then     
            return ()
       else do inpStr <- hGetLine inh
               hPutStrLn outh (map toUpper inpStr)
               mainloop inh outh

checkname :: String -> String
checkname s 
    | s == "jeff" = "Haskell is great"
    | s == "simon" = "Haskell is ok"
    | s == "greg" = "Haskell sucks"
    | otherwise = "I'm not sure"
  
