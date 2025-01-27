module Moves where

import qualified Board as B
import Data.Matrix

checkMove :: B.Board -> B.Move -> Bool
checkMove b (B.PlaceFlat (pos, _)) = checkPlace b pos 
checkMove b (B.PlaceStanding (pos, _)) = checkPlace b pos 
checkMove b (B.PlaceCap (pos, _)) = checkPlace b pos 
checkMove b s = checkSlide b s

checkPlace :: B.Board -> B.Position -> Bool
checkPlace b (B.Position row col)
  | row < 1 || col < 1 = False
  | row > nrows b || col > ncols b = False
  | otherwise = null $ getElem row col b

checkSlide :: B.Board -> B.Move -> Bool
checkSlide _ (B.PlaceFlat _ ) = False
checkSlide _ (B.PlaceStanding  _) = False
checkSlide _ (B.PlaceCap _ ) = False
checkSlide b (B.Slide (pos@(B.Position row col), count, dir, drops, color, crush))
  | null drops = False
  | td /= count || count > ncols b = False
  | any (< 1) drops = False
  | count < 1 || count > length (getElem row col b) = False
  | dir == B.Up && row - dl < 1 = False
  | dir == B.Down && row + dl > nrows b = False
  | dir == B.Left && col - dl < 1 = False
  | dir == B.Right && col + dl > ncols b = False
  | color /= B.pc (head $ getElem row col b) = False
  | checkForCap b pos dir dl = False
  | checkForStanding b pos dir dl lps = False
  | crush && checkForCrush b pos dir ld lps = False
  | otherwise = True
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
