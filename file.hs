
import System.IO
import System.Directory
import Data.List
import Data.Char

main = do   
    files <- getDirectoryContents "."
    showFiles (map (map toUpper) (sort files))

showFiles :: [FilePath] -> IO ()
showFiles (f:fs) = 
    do putStrLn f
       showFiles fs

