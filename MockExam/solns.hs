import Data.Char
import Data.List
import Control.Monad
import Test.QuickCheck

isFacecard :: Char -> Bool
isFacecard = (`elem` "AKQJ")

isNumcard :: Char -> Bool
isNumcard = (`elem` '0' : ['2'..'9'])

isCard :: Char -> Bool
isCard c = isNumcard c || isFacecard c

isFaceCard' :: Char -> Bool
isFaceCard' x = x == 'A' || x == 'K' || x == 'Q' || x == 'J'

isCard' :: Char -> Bool
isCard' x = isFaceCard' x || (isDigit x && x /= '1')

f :: String -> Bool
f str = and [isFacecard c | c <- str, isCard c]

g :: String -> Bool
g [] = True
g (c:str)
  | isCard c  = isFacecard c && g str
  | otherwise = g str

h :: String -> Bool
h = all isFacecard . filter isCard

t :: [a] -> [a]
t xs = concat [if even i then [x] else x:[x] | (x, i) <- zip xs [0..]]

u :: [a] -> [a]
u []       = []
u (x : []) = [x]
u (x:y:zs) = x : y : y : u zs

data  Proposition  =   Var String
       |   F
       |   T
       |   Not Proposition
       |   Proposition :|: Proposition
       |   Proposition :&: Proposition
       deriving (Eq, Ord, Show)

instance Arbitrary Proposition where
  arbitrary = sized expr
    where
      expr 0 =
        oneof [return F,
               return T,
               liftM Var (elements ["p", "q", "r", "s", "t"])]
      expr n | n > 0 =
        oneof [return F,
               return T,
               liftM Var (elements ["p", "q", "r", "s", "t"]),
               liftM Not (expr (n-1)),
               liftM2 (:&:) (expr (n `div` 2)) (expr (n `div` 2)),
               liftM2 (:|:) (expr (n `div` 2)) (expr (n `div` 2))]


isNorm :: Proposition -> Bool
isNorm (Not (Var _)) = True
isNorm (Not p)       = False
isNorm (p :|: q)     = isNorm p && isNorm q
isNorm (p :&: q)     = isNorm p && isNorm q
isNorm _             = True

norm :: Proposition -> Proposition
norm (Not T)         = F
norm (Not F)         = T
norm (Not (Not p))   = norm p
norm (Not (p :|: q)) = norm (Not p) :&: norm (Not q)
norm (Not (p :&: q)) = norm (Not p) :|: norm (Not q)
norm (p :|: q)       = norm p :|: norm q
norm (p :&: q)       = norm p :&: norm q
norm p               = p
