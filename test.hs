import System.Environment
import Control.Monad.Trans.State

type Person = String

father:: Person -> Maybe Person 
father f 
    | f == "Jeff" = Just "Jack"
    | f == "Jack" = Just "Jack D."
    | otherwise = Nothing

mother :: Person -> Maybe Person
mother m
    | m == "Jeff" = Just "Bobbie"
    | m == "Bobbie" = Just "Cappy"
    | otherwise = Nothing


main = do   
    doIt

doIt :: IO()

doIt = do
    args <- getArgs
    let f = father "Jeff" >>= father
    let str =  show f
    let s = doItAgain str
    putStrLn s
    
doItAgain :: String -> String

doItAgain s = "Hello " ++ s

