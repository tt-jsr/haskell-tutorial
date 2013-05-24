
import System.IO
import System.Directory
import Data.List

main = do   
    files <- getDirectoryContents "."
    showFiles (sort files)

showFiles :: [FilePath] -> IO ()
showFiles (f:fs) = 
    do putStrLn f
       showFiles fs

  
