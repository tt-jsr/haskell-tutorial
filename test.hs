import System.Environment

data Foo = Foo {
    foo_x :: Integer,
    foo_y :: Integer,
    foo_name :: String
}

data Man = Man {
    man_height :: Integer,
    man_weight :: Double,
    man_name :: String
}

data Choo = Choo {
    choo_arg1 :: Int,
    choo_arg2 :: Int
}

main = do   
    args <- getArgs
    let x = create (head args)
    putStrLn $ get x

create :: String -> Either Foo Man
--create s = if s == "man" 
            --then Right $ Man 60 180 "Jeff"
            --else Left $ Foo 1 1 "position"
create s
    |s=="man" = Right $ Man 60 180 "Jeff"
    |s=="foo" = Left $ Foo 1 1 "position"
    |otherwise = Left $ Foo 1 1 "otherwise"

get :: Either Foo Man -> String
get s = case s of
    (Left foo) -> foo_name foo
    (Right man) -> man_name man

