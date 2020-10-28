module Tutorial5 where

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. Map
-- a.
doubles :: [Int] -> [Int]
doubles xs = map dbles xs
    where 
        dbles x = x*2

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map pencepound xs
    where 
        pencepound x = fromIntegral(x) / 100  

-- c.
uppers :: String -> String
uppers xs = map toUpper xs

-- d.
uppersComp :: String -> String
uppersComp xs = [toUpper x | x <- xs]

prop_uppers :: String -> Bool
prop_uppers s = uppers s == uppersComp s

-- 2. Filter
-- a.
alphas :: String -> String
alphas xs = filter isAlpha xs

-- b.
above :: Int -> [Int] -> [Int]
above y xs = filter aboves xs
    where 
        aboves x = y < x

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter pairs xs
    where 
        pairs (a,b) = a /= b


-- d.
rmChar :: Char -> String -> String
rmChar y xs = filter (/= y) xs

-- e.
rmCharComp :: Char -> String -> String
rmCharComp y xs = [ x | x <- xs, x /=y ]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c s = rmChar c s == rmCharComp c s


-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map dbles (filter above xs)
    where 
        dbles x = x * 2 
        above x = x > 3

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map revers (filter evenlen strs)
    where
        revers x = reverse x  
        evenlen x = even (length x)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs

-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs)   | x == True = True && andRec xs
                | otherwise = False

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (xs:xss) = xs ++ concatRec xss

concatFold :: [[a]] -> [a]
concatFold xss = foldr (++) [] xss

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec "" str2 = str2
rmCharsRec (c:str1) str2 = rmCharsRec str1 (rmChar c str2)

rmCharsFold :: String -> String -> String
rmCharsFold str1 str2 = foldr (rmChar) str2 str1

prop_rmChars :: String -> String -> Bool
prop_rmChars str1 str2 = rmCharsRec str1 str2 == rmCharsFold str1 str2

-- Matrix multiplication

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform xs = all (== head xs) xs

-- b.
valid :: Matrix -> Bool
valid matrix = prop_1 && prop_2
  where
    prop_1 = uniform (map length matrix)
    prop_2 = not (null matrix) && not (null (head matrix))

-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = length (head m) 

matrixHeight :: Matrix -> Int
matrixHeight m = length m

plusM :: Matrix -> Matrix -> Matrix
plusM m n = if (not (valid m && valid n)) && (matrixHeight m /= matrixHeight n) && (matrixWidth m /= matrixWidth n) then error "Matrix incorrect" else zipWith (zipWith (+)) m n

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM m n = if (not (valid m && valid n)) && (matrixWidth m /= matrixWidth n) then error "Matrix incorrect" else zipWith (zipWith (*)) m n

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [ f x y | (x,y) <- zip xs ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f)(zip xs ys) 

-- ** Optional material

-- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f m = [map f x | x <- m]

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f m n = [zipWith f a b | (a,b) <- zip m n] 

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
