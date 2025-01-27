module Moves where

import qualified Board as B
import Data.Matrix

newtype InvalidMove = InvalidMove String
  deriving (Show, Eq)

-------------------------
-- | Move Validation | --
-------------------------
checkMove :: B.Board -> B.Move -> Either InvalidMove Bool
checkMove b (B.PlaceFlat (pos, _)) = checkPlace b pos 
checkMove b (B.PlaceStanding (pos, _)) = checkPlace b pos 
checkMove b (B.PlaceCap (pos, _)) = checkPlace b pos 
checkMove b s = checkSlide b s

checkPlace :: B.Board -> B.Position -> Either InvalidMove Bool
checkPlace b (B.Position row col)
  | row < 1 || col < 1 = Left $ InvalidMove "Invalid Position (1, 1) or greater"
  | row > nrows b || col > ncols b = Left $ InvalidMove "Invalid Position (nrows, ncols) or less"
  | otherwise = Right $ null $ getElem row col b

checkSlide :: B.Board -> B.Move -> Either InvalidMove Bool
checkSlide _ (B.PlaceFlat _ ) = Left $ InvalidMove "Can't Slide a PlaceFlat"
checkSlide _ (B.PlaceStanding  _) = Left $ InvalidMove "Can't Slide a PlaceStanding"
checkSlide _ (B.PlaceCap _ ) = Left $ InvalidMove "Can't Slide a PlaceCap"
checkSlide b (B.Slide (pos@(B.Position row col), count, dir, drops, color, crush))
  | row < 1 || col < 1 = Left $ InvalidMove "Invalid Position (1, 1) or greater"
  | row > nrows b || col > ncols b = Left $ InvalidMove "Invalid Position (nrows, ncols) or less"
  | null drops = Left $ InvalidMove "No Drops"
  | td /= count || count > ncols b = Left $ InvalidMove "Invalid Count or Drops"
  | any (< 1) drops = Left $ InvalidMove "Invalid Drop"
  | count < 1 || count > length (getElem row col b) = Left $ InvalidMove "Invalid Count For Stack"
  | dir == B.Up && row - dl < 1 = Left $ InvalidMove "Not Enough Rows Up"
  | dir == B.Down && row + dl > nrows b = Left $ InvalidMove "Not Enough Rows Down"
  | dir == B.Left && col - dl < 1 = Left $ InvalidMove "Not Enough Columns Left"
  | dir == B.Right && col + dl > ncols b = Left $ InvalidMove "Not Enough Columns Right"
  | color /= B.pc (head $ getElem row col b) = Left $ InvalidMove "Color Does Not Control Stack"
  | checkForCap b pos dir dl = Left $ InvalidMove "Cap In The Way"
  | checkForStanding b pos dir dl lps = Left $ InvalidMove "Standing In The Way"
  | crush && checkForCrush b pos dir ld lps = Left $ InvalidMove "Crush not set correctly"
  | otherwise = Right True
  where
    dl = length drops
    td = sum drops
    ld = last drops
    lps = drop (dl - ld) (getElem row col b)

checkForCap :: B.Board -> B.Position -> B.Direction -> Int -> Bool
checkForCap _ _ _ 0 = False
checkForCap b (B.Position row col) dir dl
  | null (getElem row' col' b) = checkForCap b newPos dir (dl - 1)
  | B.ps (head $ getElem row' col' b) == B.Cap = True
  | otherwise = checkForCap b newPos dir (dl - 1)
  where
    (row', col') = case dir of
      B.Up -> (row - 1, col)
      B.Down -> (row + 1, col)
      B.Left -> (row, col - 1)
      B.Right -> (row, col + 1)
    newPos = B.Position row' col'

checkForStanding :: B.Board -> B.Position -> B.Direction -> Int -> [B.Piece]-> Bool
checkForStanding _ _ _ 0 _ = False
checkForStanding b (B.Position row col) dir 1 ps
  | null (getElem row' col' b) = False
  | topStanding b newPos = length ps == 1 && lastCap ps
  | otherwise = False
  where
    (row', col') = case dir of
      B.Up -> (row - 1, col)
      B.Down -> (row + 1, col)
      B.Left -> (row, col - 1)
      B.Right -> (row, col + 1)
    newPos = B.Position row' col'
checkForStanding b (B.Position row col) dir dl ps
  | null (getElem row' col' b) = checkForStanding b newPos dir (dl - 1) ps
  | topStanding b (B.Position (row - 1) col) = True
  | otherwise = checkForStanding b newPos dir (dl - 1) ps
  where
    (row', col') = case dir of
      B.Up -> (row - 1, col)
      B.Down -> (row + 1, col)
      B.Left -> (row, col - 1)
      B.Right -> (row, col + 1)
    newPos = B.Position row' col'

checkForCrush :: B.Board -> B.Position -> B.Direction -> Int -> [B.Piece] -> Bool
checkForCrush _ _ _ _ [] = False
checkForCrush b (B.Position row col) dir dl [B.Piece _ B.Cap]
  | null (getElem (row - dl) col b) = False
  | topStanding b newPos = True
  | otherwise = False
  where
    newPos = case dir of
      B.Up -> B.Position (row - 1) col
      B.Down -> B.Position (row + 1) col
      B.Left -> B.Position row (col - 1)
      B.Right -> B.Position row (col + 1)
checkForCrush _ _ _ _ _ = False

topStanding :: B.Board -> B.Position -> Bool
topStanding b (B.Position row col)
  | null (getElem row col b) = False
  | otherwise = B.ps (head $ getElem row col b) == B.Standing

lastCap :: [B.Piece] -> Bool
lastCap [] = False
lastCap ps = B.ps (last ps) == B.Cap

----------------------------
-- | Make and Undo Move | --
----------------------------
makeMove :: B.Board -> B.Move -> Either InvalidMove B.Board
makeMove b m@(B.PlaceFlat (pos, c)) = 
  case checkMove b m of
    Left e -> Left e
    Right _ -> Right $ B.placeFlat b pos c
makeMove b m@(B.PlaceStanding (pos, c)) =
  case checkMove b m of
    Left e -> Left e
    Right _ -> Right $ B.placeStanding b pos c
makeMove b m@(B.PlaceCap (pos, c)) =
  case checkMove b m of
    Left e -> Left e
    Right _ -> Right $ B.placeCap b pos c
makeMove b m@(B.Slide (B.Position row col, count, dir, drops, _, crush)) =
  case checkMove b m of
    Left e -> Left e
    Right _ -> Right $ makeSlide b' dir pos' ps drops crush
  where
    ps = take count $ getElem row col b
    s' = drop count $ getElem row col b
    b' = setElem s' (row, col) b
    pos' = case dir of
      B.Up -> B.Position (row - 1) col
      B.Down -> B.Position (row + 1) col
      B.Left -> B.Position row (col - 1)
      B.Right -> B.Position row (col + 1)


makeSlide :: B.Board -> B.Direction -> B.Position -> [B.Piece] -> [Int] -> B.Crush -> B.Board
makeSlide b _ _ _ [] _ = b
makeSlide b _ pos@(B.Position row col) [x] _ True =
  let s = getElem row col b
      s' = drop 1 s
      h' = case B.pc (head s) of
          B.Black -> B.Piece B.Black B.Flat
          B.White -> B.Piece B.White B.Flat
  in setElem ( x : h' : s') (row, col) b

  -- where
  --   nextPos = case dir of
  --     B.Up -> B.Position (row - 1) col
  --     B.Down -> B.Position (row + 1) col
  --     B.Left -> B.Position row (col - 1)
  --     B.Right -> B.Position row (col + 1)


data InvalidUndo = InvalidPlaceUndo | InvalidSlideUndo | InvalidUndoPosition

undoMove :: B.Board -> B.Move -> Either InvalidUndo B.Board
undoMove b (B.PlaceFlat (B.Position row col, _))
  | row < 1 || col < 1 || row > nrows b || col > ncols b = Left InvalidUndoPosition
  | null (getElem row col b) = Left InvalidPlaceUndo
  | length (getElem row col b) > 1 = Left InvalidPlaceUndo
  | otherwise = Right $ setElem [] (row, col) b
undoMove b (B.PlaceStanding (B.Position row col, _))
  | row < 1 || col < 1 || row > nrows b || col > ncols b = Left InvalidUndoPosition
  | null (getElem row col b) = Left InvalidPlaceUndo
  | length (getElem row col b) > 1 = Left InvalidPlaceUndo
  | otherwise = Right $ setElem [] (row, col) b
undoMove b (B.PlaceCap (B.Position row col, _))
  | row < 1 || col < 1 || row > nrows b || col > ncols b = Left InvalidUndoPosition
  | null (getElem row col b) = Left InvalidPlaceUndo
  | length (getElem row col b) > 1 = Left InvalidPlaceUndo
  | otherwise = Right $ setElem [] (row, col) b

