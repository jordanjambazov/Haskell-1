map' :: (a -> a) -> [a] -> [a]
map' fn    (x:xs)     = (fn x) : (map' fn xs)
map' _     _          = []

filter' :: (a -> Bool) -> [a] -> [a]
filter' fn  (x:xs)    | (fn x)      = x : filter' fn xs
filter' fn  (x:xs)    | otherwise   = filter' fn xs
filter' _   _                       = []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc []     = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs
