{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

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

-- 08. Map a function to a list of lists
mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap f []  = []
mapMap f (x:xs) = (map f x) : (mapMap f xs)

-- 09. Filter a list of lists
filterFilter :: (a -> Bool) -> [[a]] -> [[a]]
filterFilter f []  = []
filterFilter f (x:xs) = (filter f x) : (filterFilter f xs)

-- 10. Generate the unit matrix by given element and dimensions
unit :: (Num a1, Num a, Ord a1) => a -> a1 -> [[a]]
unit item size = matrix 0 []
  where row index num acc    | num == index  = item : (row (index + 1) num acc)
                             | index < size  = 0 : row (index + 1) num acc
                             | otherwise     = acc
        matrix index acc     | index < size = (row 0 index []):(matrix (index + 1) acc)
                             | otherwise = acc

-- 11. Get the nth row and column of a matrix
row :: Int -> [[a]] -> [a]
row nth xs = xs !! nth

-- 12. Transpose a matrix
transpose' :: [[a]] -> [[a]]
transpose' ([]:xs) = []
transpose' m = map head m : transpose' (map tail m)

-- 13. Sum of matrices
sumMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
sumMatrices (x:xs) (y:ys) = map (\(a, b) -> a + b) (zip x y) : sumMatrices xs ys
sumMatrices _      _      = []

-- 14. Multiply matrices
multMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
multMatrices a b = [ [ sum $ zipWith (*) ar bc | bc <- transpose' b ] | ar <- a ]

-- 15. Histogram
histogram :: [Int] -> IO()
histogram xs = putStr [ a | a <- '*', ] ++ "\n0123456789\n"

countNum n xs = length (filter (== n) xs)
