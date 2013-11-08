-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 7/8 November

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen = recur_len 0
  where
    recur_len :: Int -> [(Barcode, Item)] -> Int
    recur_len max [] = max
    recur_len max ((k, (p, u)):rest)
      | length p > max = recur_len (length p) rest
      | otherwise      = recur_len max rest

formatLine :: Int -> (Barcode, Item) -> String
formatLine longest (bc, (p, u)) = bc ++ "..." ++ p ++ replicate pad '.' ++ "..." ++ u
  where
    pad = longest - length p

showCatalogue :: Catalogue -> String
showCatalogue c = unlines $ map (formatLine longest) pairs
  where
    pairs   = toList c
    longest = longestProductLen pairs

     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just e) = [e]

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr maybe_append []
  where
    maybe_append Nothing l  = l
    maybe_append (Just e) l = e : l

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems codes catalog = catMaybes $ map cata_lookup codes
  where
    cata_lookup code = get code catalog






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
