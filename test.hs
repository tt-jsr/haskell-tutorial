import System.Environment

data Foo = Foo {
    foo_x :: Integer, 
    foo_y :: Integer, 
    foo_name :: String
}

data Man = Man {
    man_height :: Integer, 
    man_weight :: Integer, 
    man_name :: String
}

data Choo = Choo {
    choo_arg1 :: Integer, 
    choo_arg2 :: Integer
}


data Quad a b c d = First a | Second b | Third c | Fourth d

main = do   
    args <- getArgs
    let x = create (if length args == 0 then "No argument" else head args)
    putStrLn $ get x

create :: String -> Quad Foo Man Choo String
create s
    |s=="foo" = First (Foo 1 1 "position")
    |s=="man" = Second (Man 60 180 "Jeff")
    |s=="choo" = Third (Choo 10 20)
    |otherwise = Fourth s

get :: Quad Foo Man Choo String  -> String
get e = case e of
    First first -> "Have a foo: " ++ (foo_name first)
    Second sec -> "Have a man: " ++ ( man_name sec)
    Third third -> "Have a choo: " ++ show (choo_arg1 third) ++ "," ++ show (choo_arg2 third)
    Fourth fourth -> "Error: " ++ fourth

