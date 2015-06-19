-- Solutions
import Data.Char (chr, ord)

-- 01. Even
even' :: Int -> Bool
even' number = number `mod` 2 == 0

-- 02. Odd
odd' :: Int -> Bool
odd' number = not (even' number)

-- 03. Calculate BMI
bmi :: Double -> Double -> Double
bmi height weight = weight / (height ^ 2)

-- 04. Convert degrees to radians
deg2Rad :: Double -> Double
deg2Rad degrees = (degrees / 180) * pi

-- 05. Convert radians to degrees
rad2Deg :: Double -> Double
rad2Deg radians = (radians * 180) / pi

-- 06. Does it form a triangle?
isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c = (c + b) > a && (a + c) > b && (a + b) > c

-- 07. Perimeter of a triangle
perimeter, halfperimeter :: [Double] -> Double
perimeter xs = sum xs
halfperimeter xs = (perimeter xs) / 2

-- 08. Area of a triangle
area :: Double -> Double -> Double -> Double
area a b c = sqrt (p * ((p-a)
             *(p - b)
             *(p - c)))
             where p = halfperimeter [a, b, c]

-- 09. Calculate
calculate :: Num a => Char -> a -> a -> a
calculate op x y = if op == '+'
                   then x + y else
                   if op == '-'
                   then x - y else
                   if op == '*'
                   then x * y else
                   error "Unknown operator!"

-- 10. Currency Converter (working with dollars, levs and euros)
convert :: Num a => [Char] -> [Char] -> a -> a
convert "bgn" "usd" value = value * 42
convert "usd" "bgn" value = value * 42
convert "eur" "usd" value = value * 42
convert _     _     value = error "Invalid currency"

-- 11. Type signatures
{-
Type signatures for the 10 tasks above have been rewritten. All solutions from
now on will have signatures.
-}

-- 12. Rewrite the triangle functions from above to get lists as arguments
{-
The `perimeter` function has been rewritten to accept list for arguments.
-}

-- 13. Head
head' :: [a] -> a
head' [] = error "Empty list"
head' (x:_) = x

-- 14. Tail
tail' :: [a] -> [a]
tail' [] = error "Empty list"
tail' (x:xs) = xs

-- 15. We love pattern matching!
{-
Check functions above. Pattern matching is used when appropriate.
-}

-- 16. Last
last' :: Eq a => [a] -> a
last' xs = if (t == []) then
           head' xs else
           last' t
           where t = tail' xs

-- 17. Double all elements
double :: [Double] -> [Double]
double [] = []
double (x:xs) = x*2:(double xs)

-- 18. More generic - make it possible to multiply all elements in a list
mult _ [] = []
mult multiplier (x:xs) = x*multiplier:(mult multiplier xs)

-- 19. Get the n-th element of a list
nth num (x:xs) = if num < 1 then
                 x else
                 nth (num-1) xs

firstTuple (x,_,_) = x
first2Tuple (x,_) = x

secondTuple (_,y,_) = y
second2Tuple (_,y) = y

thirdTuple (_,_,z) = z

-- 20. Is an element member of a list?
member item xs = any (==item) xs

-- 21. Is the list a fibonacci sequence?
isFibonacciSequence (x:y:xs)
  | xs == [] = True
  | x + y == head xs = isFibonacciSequence (y:xs)
  | x + y /= head xs = False
  | otherwise  = False

-- 22. Get the sum of a list
sum' (x:xs) = x + sum' xs

-- 23. Get the product of a list
product' (x:xs) = x * (product' xs)

-- 24. Multiply lists
multLists :: Num a => [a] -> [a] -> [a]
multLists first second = zipWith (*) first second

-- 25. Number to string
number2string :: Int -> [Char]
number2string n = show n :: [Char]

-- 26. String to number
string2number :: [Char] -> Int
string2number s = read s :: Int

-- 27. Is valid ID?
enumerate x = zip [0..] x

withoutLast (x:[]) = []
withoutLast (x:xs) =
  (x:withoutLast xs)

isValidID :: [Char] -> Bool
isValidID egn =
  last' egn == chr ((controlDigit (withoutLast egn)) + 48)
  where
    positionWeight pos = nth (pos) [2, 4, 8, 5, 10, 9, 7, 3, 6]
    multEach (pos, c) = (string2number [c]) * (positionWeight pos)
    sumEGN egn = sum (map multEach (enumerate egn))
    residue egn = (sumEGN egn) `mod` 11
    controlDigit egn =
      if residue egn < 10 then residue egn else 0

-- 28. Get the zodiac sign from an ID
whatZodiacSignIs :: Int -> Int -> [Char]
whatZodiacSignIs day month =
  findZodiac signs
  where signs = [
            ("Овен",      (21, 3),  (20, 4)),
            ("Телец",     (21, 4),  (20, 5)),
            ("Близнаци",  (21, 5),  (20, 6)),
            ("Рак",       (21, 6),  (21, 7)),
            ("Лъв",       (22, 7),  (22, 8)),
            ("Дева",      (23, 8),  (22, 9)),
            ("Везни",     (23, 9),  (22, 10)),
            ("Скорпион",  (23, 10), (21, 11)),
            ("Стрелец",   (22, 11), (21, 12)),
            ("Козирог",   (22, 12), (19, 1)),
            ("Водолей",   (20, 1),  (18, 2)),
            ("Риби",      (19, 2),  (20, 3))
          ]
        signName sign = firstTuple sign
        startDate sign = secondTuple sign
        endDate sign = thirdTuple sign
        getDay date = first2Tuple date
        getMonth date = second2Tuple date
        dateInSign sign =
          (
            (day >= getDay (startDate sign)) && (month == getMonth (startDate sign))
          )
          ||
          (
            (day <= getDay (endDate sign)) && (month == getMonth (startDate sign))
          )
        findZodiac signsList =
          if dateInSign (head' signsList) then
            signName (head' signsList)
          else
            findZodiac (tail' signsList)

-- 29. More type signatures!
-- TODO

-- 30. Concatenate the lists
concatenate :: [a] -> [a] -> [a]
concatenate listOne listTwo = listOne ++ listTwo

-- 31. Take all elements of a list without the last one
init' :: [a] -> [a]
init' [x]    = []
init' (x:xs) = x : init' xs
init' []     = error "You can't do that with the empty list!"

-- 32. Take the first n elements from a list
take' :: Integer -> [a] -> [a]
take' _ []               = []
take' n _      | n <= 0  = []
take' n (x:xs)           = x : take' (n-1) xs

-- 33. Drop the first n elements from a list
drop' :: Int -> [a] -> [a]
drop' n xs     | n <= 0  = xs
drop' n []               = []
drop' n (_:xs)           = drop (n-1) xs

-- 34. Zipping lists
zip' :: [a] -> [b] -> [(a, b)]
zip' []     _            = []
zip' _      []           = []
zip' (a:as) (b:bs)       = (a, b) : zip' as bs

-- 35. Now unzip it!
unzip' [] = ([], [])
unzip' ((a, b):xs) = (a : (first2Tuple rest), b : (second2Tuple rest))
    where rest = unzip' xs
