-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 10/11 Oct.

import Data.Char
import Data.List
import Data.Ord
import Data.Function
import Data.List.Split
import Test.QuickCheck
import Control.Monad


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n str = drop n str ++ take n str

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey n = zip alphabet $ rotate n alphabet
  where
    alphabet = ['A'..'Z']

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c tuples
  | null matches = c
  | otherwise    = head matches
    where
      matches    = [snd t | t <- tuples, fst t == c]

-- 5.
encipher :: Int -> Char -> Char
encipher n c = lookUp c $ makeKey n

-- 6.
normalize :: String -> String
normalize str = [toUpper c | c <- str, isLetter c]

-- 7.
encipherStr :: Int -> String -> String
encipherStr n str = [encipher n c | c <- normalize str]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey tuples = [(b, a) | (a, b) <- tuples]

-- 9.
decipher :: Int -> Char -> Char
decipher n c = lookUp c $ reverseKey . makeKey $ n

decipherStr :: Int -> String -> String
decipherStr n str = [decipher n c | c <- str]

-- 10.
contains :: String -> String -> Bool
contains [] n = null n
contains haystack@(h:hs) n
  | n `isPrefixOf` haystack = True
  | otherwise               = contains hs n

-- 11.
candidates :: String -> [(Int, String)]
candidates string = [(n, candidate n) | n <- [1..26],
                      candidate n `contains` "AND" || candidate n `contains` "THE"]
  where
    candidate n = decipherStr n string



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive "" = []
splitEachFive str
  | length str >= 5 = take 5 str : splitEachFive (drop 5 str)
  | otherwise       = [take 5 $ str ++ cycle "X"]

-- 13.
prop_transpose :: String -> Bool
prop_transpose = liftM2 (||) null (liftM2 (==) repeats (transpose . transpose . repeats))
  where repeats str = take 5 $ cycle [str]

-- 14.
encrypt :: Int -> String -> String
encrypt n str = concat $ transpose $ splitEachFive $ encipherStr n str

-- 15.
decrypt :: Int -> String -> String
decrypt n str = concat $ transpose $ chunksOf (length str `div` 5) $ decipherStr n str

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs string = map (reduceBucket id (+) 0) $ groupTuples $ map (\c -> (c, 1)) string
  where
    groupTuples = groupBy (on (==) fst) . sort
    reduceBucket key_f reduce_f base group = (key_f key, foldr (reduce_f . snd) base group)
      where key = fst $ head group

-- 17
freqDecipher :: String -> [String]
freqDecipher string = map ((`decrypt` string) . delta) e_candidates
  where
    e_candidates  = map fst $ sortBy ((flip . comparing) snd) (countFreqs string)
    delta char    = (ord char - ord_e) `mod` 26
      where ord_e = 69
