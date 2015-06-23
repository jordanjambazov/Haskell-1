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
--every :: Integral -> [a] -> [a]
--every n l = helper 0 l
--       where helper i (x:xs)  | (i `mod` n) == 0    = helper (i + 1) xs

