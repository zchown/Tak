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
checkForCap b (B.Position row col) B.Up dl
  | null (getElem (row - 1) col b) = checkForCap b (B.Position (row - 1) col) B.Up (dl - 1)
  | B.ps (head $ getElem (row - 1) col b) == B.Cap = True
  | otherwise = checkForCap b (B.Position (row - 1) col) B.Up (dl - 1)
checkForCap b (B.Position row col) B.Down dl
  | null (getElem (row + 1) col b) = checkForCap b (B.Position (row + 1) col) B.Down (dl - 1)
  | B.ps (head $ getElem (row + 1) col b) == B.Cap = True
  | otherwise = checkForCap b (B.Position (row + 1) col) B.Down (dl - 1)
checkForCap b (B.Position row col) B.Left dl
  | null (getElem row (col - 1) b) = checkForCap b (B.Position row (col - 1)) B.Left (dl - 1)
  | B.ps (head $ getElem row (col - 1) b) == B.Cap = True
  | otherwise = checkForCap b (B.Position row (col - 1)) B.Left (dl - 1)
checkForCap b (B.Position row col) B.Right dl
  | null (getElem row (col + 1) b) = checkForCap b (B.Position row (col + 1)) B.Right (dl - 1)
  | B.ps (head $ getElem row (col + 1) b) == B.Cap = True
  | otherwise = checkForCap b (B.Position row (col + 1)) B.Right (dl - 1)

checkForStanding :: B.Board -> B.Position -> B.Direction -> Int -> [B.Piece]-> Bool
checkForStanding _ _ _ 0 _ = False
checkForStanding b (B.Position row col) B.Up 1 ps
  | null (getElem (row - 1) col b) = False
  | topStanding b (B.Position (row - 1) col) = length ps == 1 && lastCap ps
  | otherwise = False
checkForStanding b (B.Position row col) B.Down 1 ps
  | null (getElem (row + 1) col b) = False
  | topStanding b (B.Position (row + 1) col) = length ps == 1 && lastCap ps
  | otherwise = False
checkForStanding b (B.Position row col) B.Left 1 ps
  | null (getElem row (col - 1) b) = False
  | topStanding b (B.Position row (col - 1)) = length ps == 1 && lastCap ps
  | otherwise = False
checkForStanding b (B.Position row col) B.Right 1 ps
  | null (getElem row (col + 1) b) = False
  | topStanding b (B.Position row (col + 1)) = length ps == 1 && lastCap ps
  | otherwise = False
checkForStanding b (B.Position row col) B.Up dl ps
  | null (getElem (row - 1) col b) = checkForStanding b (B.Position (row - 1) col) B.Up (dl - 1) ps
  | topStanding b (B.Position (row - 1) col) = True
  | otherwise = checkForStanding b (B.Position (row - 1) col) B.Up (dl - 1) ps
checkForStanding b (B.Position row col) B.Down dl ps
  | null (getElem (row + 1) col b) = checkForStanding b (B.Position (row + 1) col) B.Down (dl - 1) ps
  | topStanding b (B.Position (row + 1) col) = True
  | otherwise = checkForStanding b (B.Position (row + 1) col) B.Down (dl - 1) ps
checkForStanding b (B.Position row col) B.Left dl ps
  | null (getElem row (col - 1) b) = checkForStanding b (B.Position row (col - 1)) B.Left (dl - 1) ps
  | topStanding b (B.Position row (col - 1)) = True
  | otherwise = checkForStanding b (B.Position row (col - 1)) B.Left (dl - 1) ps
checkForStanding b (B.Position row col) B.Right dl ps
  | null (getElem row (col + 1) b) = checkForStanding b (B.Position row (col + 1)) B.Right (dl - 1) ps
  | topStanding b (B.Position row (col + 1)) = True
  | otherwise = checkForStanding b (B.Position row (col + 1)) B.Right (dl - 1) ps

checkForCrush :: B.Board -> B.Position -> B.Direction -> Int -> [B.Piece] -> Bool
checkForCrush _ _ _ _ [] = False
checkForCrush b (B.Position row col) B.Up dl [B.Piece _ B.Cap]
  | null (getElem (row - dl) col b) = False
  | topStanding b (B.Position (row - 1) col) = True
  | otherwise = False
checkForCrush b (B.Position row col) B.Down dl [B.Piece _ B.Cap]
  | null (getElem (row + dl) col b) = False
  | topStanding b (B.Position (row + 1) col) = True
  | otherwise = False
checkForCrush b (B.Position row col) B.Left dl [B.Piece _ B.Cap]
  | null (getElem row (col - dl) b) = False
  | topStanding b (B.Position row (col - 1)) = True
  | otherwise = False
checkForCrush b (B.Position row col) B.Right dl [B.Piece _ B.Cap]
  | null (getElem row (col + dl) b) = False
  | topStanding b (B.Position row (col + 1)) = True
  | otherwise = False
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

