-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 14/15 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (a :#: b)   = split a ++ split b
split Sit         = []
split a           = [a]

-- 1b. join
join :: [Command] -> Command
join = foldl1 (:#:)

-- 1c  equivalent
equivalent a b = split a  == split b

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent c $ join . split $ c

prop_split c = all valid $ split c
  where
    valid Sit     = False
    valid (_:#:_) = False
    valid _       = True


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy i c = join $ replicate i c

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d i = copy i (Go d :#: Turn (360.0 / fromIntegral i))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral d i d' a = join $ recSpiral d i d' a
  where
    recSpiral :: Distance -> Int -> Distance -> Angle -> [Command]
    recSpiral _ 0 _ _  = []
    recSpiral d i d' a = (Go d :#: Turn a) : recSpiral (d + d') (i - 1) d' a


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise = join . fuse . split
  where
    fuse []                      = []
    fuse (Go d : Go d' : cs)     = fuse $ Go (d + d') : fuse cs
    fuse (Go 0 : cs)             = fuse cs
    fuse (Turn a : Turn a' : cs) = fuse $ Turn (a + a') : fuse cs
    fuse (Turn 0 : cs)           = fuse cs
    fuse (c:cs)                  = c : fuse cs



-- L-Systems
-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where
      f 0 = GrabPen red :#: Go 10
      f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
      g 0 = GrabPen blue :#: Go 10
      g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
      n = Turn 60
      p = Turn(-60)


-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
    where
      f 0 = Go 10
      f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1):#: p :#: f (x-1)
      n = Turn 60
      p = Turn(-60)


-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
    where
      l 0 = Sit
      l x = p :#: r (x-1) :#: f :#: n :#: l (x-1) :#: f :#: l (x-1) :#: n :#: f :#: r (x-1) :#: p
      r 0 = Sit
      r x = n :#: l (x-1) :#: f :#: p :#: r (x-1) :#: f :#: r (x-1) :#: p :#: f :#: l (x-1) :#: n
      f = GrabPen black :#: Go 10
      n = Turn 90
      p = Turn(-90)


