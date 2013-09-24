-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Due: the tutorial of week 3 (4/5 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck


-- 1. halveEvens
-- Helper functions:
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

halve :: Int -> Int
halve x = x `div` 2

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ halve x | x <- xs, isEven x ]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
  | isEven x  = halve x : halveEvensRec xs
  | otherwise = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs


-- 2. inRange
-- Helper function:
isBetween :: Int -> Int -> Int -> Bool
isBetween x lo hi = x >= lo && x <= hi

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs, ((x `isBetween`) lo hi) ]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x:xs)
  | (x `isBetween`) lo hi = x : inRangeRec lo hi xs
  | otherwise             = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs


-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = sum [ 1 | x <- xs, x > 0 ]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
  | x > 0     = 1 + countPositivesRec xs
  | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs


-- 4. pennypincher

-- Helper functions
spendingLimit :: Int
spendingLimit = 19900

discount :: Int -> Int
discount price = round $ 0.9 * fromIntegral price

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [ discount x | x <- xs, discount x <= spendingLimit ]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs)
  | x' <= spendingLimit = x' + pennypincherRec xs
  | otherwise           = pennypincherRec xs
    where
      x' = discount x

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs


-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [ digitToInt x | x <- xs, isDigit x ]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (c:cs)
  | isDigit c = digitToInt c * multDigitsRec cs
  | otherwise = multDigitsRec cs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits cs = multDigits cs == multDigitsRec cs


-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise []     = []
capitalise (c:cs) = toUpper c : [ toLower c' | c' <- cs ]

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec []     = []
capitaliseRec (c:cs) = toUpper c : lowerRec cs

lowerRec :: String -> String
lowerRec []     = []
lowerRec (c:cs) = toLower c : lowerRec cs

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise cs = capitalise cs == capitaliseRec cs


-- 7. title
-- Helper function:
correctCase :: String -> String
correctCase word
  | length word >= 4 = capitalise word
  | otherwise        = lowerRec word
-- List-comprehension version
title :: [String] -> [String]
title []     = []
title (w:ws) = capitalise w : [ correctCase word | word <- ws ]

-- Recursive version
titleRec :: [String] -> [String]
titleRec []     = []
titleRec (w:ws) = capitalise w : correctRec ws
  where
    correctRec []     = []
    correctRec (w:ws) = correctCase w : correctRec ws

-- mutual test
prop_title :: [String] -> Bool
prop_title ws = title ws == titleRec ws


-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter inPosition len words = [ word | word <- words, isCorrectLength word, correctlyPlacedLetter word ]
  where
    isCorrectLength word       = length word == len
    correctlyPlacedLetter word = inPosition < length word && word !! inPosition == letter

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec _ _ _ [] = []
crosswordFindRec letter inPosition len (w:ws)
  | length w == len && inPosition < len && w !! inPosition == letter = w : continueSearch
  | otherwise                                                        = continueSearch
    where
      continueSearch = crosswordFindRec letter inPosition len ws

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind letter inPosition len words = crosswordFind letter (abs inPosition) len words == crosswordFindRec letter (abs inPosition) len words


-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search haystack needle = [ snd tuple | tuple <- zip haystack [0..], fst tuple == needle ]

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec = indexSearchRec 0
  where
    indexSearchRec i [] _ = []
    indexSearchRec i (h:hs) n
      | n == h    = i : indexSearchRec (i + 1) hs n
      | otherwise = indexSearchRec (i + 1) hs n

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search haystack needle = search haystack needle == searchRec haystack needle


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains haystack needle = or [ needle `isPrefixOf` drop amount haystack | amount <- [0..length haystack] ]

-- Recursive version
containsRec :: String -> String -> Bool
containsRec [] needle = null needle
containsRec haystack@(h:hs) needle
  | needle `isPrefixOf` haystack = True
  | otherwise                    = containsRec hs needle


-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains haystack needle = contains haystack needle == containsRec haystack needle

