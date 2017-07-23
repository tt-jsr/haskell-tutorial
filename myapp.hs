
type Size = Int
type Name = String

data Entry = File Name Size | Directory Name deriving (Show)

-- showEntry :: Entry -> String
show (File name size) = "File: " ++ name ++ " " ++ Prelude.show size 
show (Directory name) = "Directory: " ++ name

dir :: String -> [Entry]
dir s  
    | s == "." = (File "word.doc" 13278) : (File "home.jpg" 167343) : (Directory ".") : (Directory "..") : (Directory "Videos") : []
    | s == "Videos" = (File "mike.mov" 1367483) : (File "maegan.mov" 38596757) : (File "campfire.mov" 857473) : []
    | otherwise = Directory "null" : []

printEntries :: [Entry] -> IO String
printEntries [] = do 
    return (foo)
printEntries (x:xs) = 
    do
        putStrLn $ Main.show x 
        printEntries xs

foo :: String
foo = "Foo!"

main = do 
    putStrLn "Enter dir: "
    line <- getLine  
    let entries = dir line
    f <- printEntries entries
    putStrLn f
