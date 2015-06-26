-- 01. Map as foldl
-- Implement the function `map` using `foldl`
map' :: (a -> b) -> [a] -> [b]
map' f l = foldl (\a x -> a ++ [f x]) [] l



-- 02. Filter as foldl
filter' :: (a -> Bool) -> [a] -> [a]
filter' f l = foldl (\a x -> if f x then a ++ [x] else a) [] l

-- 03. Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:l) = quicksort [a | a <- l, a <= x] ++ [x] ++ quicksort [a | a <- l, a > x]

-- 04. Repeat
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- 05. Cycling!
cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = xs ++ cycle' xs


-- 06. Take every nth element from a list
every :: Integral a => a -> [t] -> [t]
every n l = [a | (i, a) <- zip [1..] l, i `mod` n == 0]

-- 07. Get the local maximas in a list of `Integer`s
localMaxima :: (Ord a) => [a] -> [a]
localMaxima xs = h xs []
  where h (x:y:z:xs) a | x < y && y > z = h (y:z:xs) $ a ++ [y]
                       | otherwise      = h (y:z:xs) a
        h _          a                  = a
