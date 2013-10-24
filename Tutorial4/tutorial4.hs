-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (24/25 Oct)

import Data.List
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:c.banks@ed.ac.uk\">Chris Banks</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:c.banks@ed.ac.uk\">Chris Banks</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Chris Banks","c.banks@ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

insensitive :: (String -> String -> Bool) -> String -> String -> Bool
insensitive f a b = f a' b'
  where
    a' = map toUpper a
    b' = map toUpper b

-- 1.
sameString :: String -> String -> Bool
sameString a b = map toUpper a == map toUpper b
sameString'    = insensitive (==)

-- 2.
prefix :: String -> String -> Bool
prefix n h = sameString n $ (take . length) n h
prefix'    = insensitive isPrefixOf

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr (map toLower str) &&
		      prefix substr (map toUpper str)
                          where
                            substr  =  take n str


-- 3.

contains :: String -> String -> Bool
contains [] n = null n
contains haystack@(h:hs) n
  | n `prefix` haystack = True
  | otherwise           = contains hs n

contains' :: String -> String -> Bool
contains' [] n = null n
contains' haystack@(h:hs) n
  | n `insensitivePrefixOf` haystack = True
  | otherwise                        = contains hs n
    where insensitivePrefixOf = insensitive isPrefixOf

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n m = contains (map toLower str) substr &&
  contains (map toUpper str) substr
    where
      substr = drop n $ take m $ str


-- 4.
takeUntil n "" = ""
takeUntil n haystack@(h:hs)
  | n `prefix` haystack = ""
  | otherwise           = h : takeUntil n hs

dropUntil :: String -> String -> String
dropUntil n "" = ""
dropUntil n haystack@(h:hs)
  | n `prefix` haystack = drop (length n) haystack
  | otherwise           = dropUntil n hs


-- 5.
split :: String -> String -> [String]
split sep string
  | string `contains` sep = takeUntil sep string : split sep (dropUntil sep string)
  | otherwise           = [string]

reconstruct :: String -> [String] -> String
reconstruct joiner = foldr1 join
  where join segment string = segment ++ joiner ++ string

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML = tail . split "<a href=\""

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails = filter ("mailto" `isPrefixOf`)


-- 8.
link2pair :: Link -> (Name, Email)
link2pair link = (name, email)
  where
    name = dropUntil ">" $ takeUntil "</a>" link
    email = dropUntil ":" $ takeUntil "\"" link


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML = map link2pair . takeEmails . linksFromHTML

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail partial_name = filter ((partial_name `prefix`) . fst)


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML = flip findEmail . emailsFromHTML


-- Optional Material

-- 12.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
