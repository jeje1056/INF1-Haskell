-- Informatics 1 - Functional Programming
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck
import Control.Monad

data PieceColour = Black | White
  deriving (Eq)

-- Exercise 9:

pic1 :: Picture
pic1 = left `beside` right
    where
      left  = knight `above` invert knight
      right = invert left

pic2 :: Picture
pic2 = top `above` bottom
    where
      top    = knight `beside` invert knight
      bottom = flipV top


-- Exercise 10:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 $ whiteSquare `beside` blackSquare

-- b)

otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 $ blackSquare `beside` whiteSquare

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 $ emptyRow `above` otherEmptyRow

-- d)
homeRow :: PieceColour -> Picture
homeRow colour = piecerow `over` tiles
  where
    tiles
      | colour == Black = emptyRow
      | otherwise       = otherEmptyRow
    piecerow = foldr1 beside (map modifier pieces)
    modifier
      | colour == Black = invert
      | otherwise       = id
    pieces   = wing ++ [queen, king] ++ reverse wing
    wing     = [rook, knight, bishop]

pawnRow :: PieceColour -> Picture
pawnRow colour = piecerow `over` tiles
  where
    tiles
      | colour == Black = otherEmptyRow
      | otherwise       = emptyRow
    piecerow = repeatH 8 $ modifier pawn
    modifier
      | colour == Black = invert
      | otherwise       = id

whiteRow, whitePawns, blackRow, blackPawns, whiteSegment, blackSegment, populatedBoard :: Picture
whiteRow       = homeRow White
whitePawns     = pawnRow White
blackRow       = homeRow Black
blackPawns     = pawnRow Black
whiteSegment   = whitePawns `above` whiteRow
blackSegment   = blackRow `above` blackPawns

populatedBoard = blackSegment `above` middleBoard `above` whiteSegment



-- Functions --

twoBeside :: Picture -> Picture
twoBeside = ap beside invert


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove = ap above invert

fourPictures :: Picture -> Picture
fourPictures =  twoAbove . twoBeside
