data W a = W a deriving Show

return :: a -> W a
return x = W x

fmap :: (a -> b) -> (W a -> W b)
fmap f (W x) = W (f x)

