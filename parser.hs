newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . head . apply p

instance Monad Parser where
    return x = Parser (\s -> [(x,s)])
    p >>= q = Parser (\s -> [(y,s'')
        | (x, s') <- apply p s,
        (y, s'') <- apply (q x) s'])

getc :: Parser Char
getc = Parser f
    where f [] = []
          f (c:cs) = [(c,cs)]
    
parseChar :: String -> Char
parseChar s = parse getc s

-- Experiment two
type Long = Bool
type Column = Bool
data LSArgs = LSArgs Long Column

newtype ParseArgs a = ParseArgs (String -> (a, String))

apply :: ParseArgs a -> String -> (a, String)
apply (ParseArgs p) s = p s

parse :: ParseArgs a -> String -> a
parse p = fst . head . apply p

main = putChar $ parse getc "Hello World"
    
