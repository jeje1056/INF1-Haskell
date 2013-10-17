-- Informatics 1 - Functional Programming
-- Tutorial 3
--
-- Week 5 - Due: 17/18 Oct.

import Data.Char
import Test.QuickCheck
import Control.Monad



-- 1. Map
-- a.
uppers :: String -> String
uppers = map toUpper

-- b.
doubles :: [Int] -> [Int]
doubles = map (* 2)

-- c.
penceToPounds :: [Int] -> [Float]
penceToPounds = map ((/ 100) . fromIntegral)

-- d.
uppers' :: String -> String
uppers' str = [ toUpper c | c <- str ]

prop_uppers :: String -> Bool
prop_uppers = liftM2 (==) uppers uppers'



-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
rmChar ::  Char -> String -> String
rmChar = filter . (/=)

-- c.
above :: Int -> [Int] -> [Int]
above = filter . (<)

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter $ uncurry (/=)

-- e.
rmCharComp :: Char -> String -> String
rmCharComp drop_char str = [ c | c <- str, c /= drop_char ]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c str = rmChar c str == rmCharComp c str



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' = map toUpper . filter isAlpha

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = map (* 2) . filter (> 3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' = map reverse . filter (even . length)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (b:bs) = b && andRec bs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec []       = []
concatRec (l : ls) = l ++ concatRec ls

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] s = s
rmCharsRec (c:cs) string = rmCharsRec cs $ rmChar c string

rmCharsFold :: String -> String -> String
rmCharsFold = 

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform = undefined

-- b.
valid :: Matrix -> Bool
valid = undefined

-- 6.

-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM = undefined

-- 8.
timesM :: Matrix -> Matrix -> Matrix
timesM = undefined

-- Optional material
-- 9.
