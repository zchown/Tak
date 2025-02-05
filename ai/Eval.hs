module Eval where

import qualified Board as B
import Data.Matrix
import qualified Data.Vector as V
import qualified Moves as M

roadWin = 100000

flatWin = roadWin - 1

stupidEval :: B.GameState -> Int
stupidEval gs@(B.GameState b c _ _ _ _ _) = do
  case checkForWinScore gs of
    Just score -> score
    _ ->
      length (M.controlledPositions b B.White) -
      length (M.controlledPositions b B.Black)

betterEval :: B.GameState -> Int
betterEval gs@(B.GameState b _ _ _ _ _ _) = do
  case checkForWinScore gs of
    Just score -> score
    _ -> (2 * fcd) + pc + center
  where
    fcd = getFlatCount b B.White - getFlatCount b B.Black
    pc = getPrisonerCount b B.White - getPrisonerCount b B.Black
    center =
      encourageCenterPlay b (M.controlledPositions b B.Black) -
      encourageCenterPlay b (M.controlledPositions b B.White)

bestEval :: B.GameState -> Int
bestEval gs@(B.GameState b _ _ _ _ _ _) = do
  case checkForWinScore gs of
    Just score -> score
    _ -> (7 * fcd) + (1 * pc) + (2 * buddies) + (5 * lr) + (2 * center)
  where
    whiteControlled = M.controlledPositions b B.White
    blackControlled = M.controlledPositions b B.Black
    buddies =
      getBuddies b B.White whiteControlled -
      getBuddies b B.Black blackControlled
    fcd = getFlatCount b B.White - getFlatCount b B.Black
    pc = getPrisonerCount b B.White - getPrisonerCount b B.Black
    lr = longestRoad b B.White - longestRoad b B.Black
    center =
      encourageCenterPlay b whiteControlled -
      encourageCenterPlay b blackControlled

bestEval' :: B.GameState -> Int
bestEval' gs@(B.GameState b _ _ _ _ _ _) = do
  case checkForWinScore gs of
    Just score -> score
    _ -> (11 * fcd) + (2 * pc) + (3 * buddies) + (7 * lr) + (4 * center) + wp
  where
    whiteControlled = M.controlledPositions b B.White
    blackControlled = M.controlledPositions b B.Black
    buddies =
      getBuddies b B.White whiteControlled -
      getBuddies b B.Black blackControlled
    fcd = getFlatCount b B.White - getFlatCount b B.Black
    pc = getPrisonerCount b B.White - getPrisonerCount b B.Black
    lr = longestRoad b B.White - longestRoad b B.Black
    center =
      encourageCenterPlay b whiteControlled -
      encourageCenterPlay b blackControlled
    wp =
      winnablePositions b B.White whiteControlled -
      winnablePositions b B.Black blackControlled

--------------------------
-- | Helper Functions | --
---------------------------
checkForWinScore :: B.GameState -> Maybe Int
checkForWinScore gs
  | r == B.Road B.White = Just $ roadWin
  | r == B.Road B.Black = Just $ -roadWin
  | r == B.FlatWin B.White = Just $ flatWin
  | r == B.FlatWin B.Black = Just $ -flatWin
  | otherwise = Nothing
  where
    r = B.checkGameResult gs

getFlatCount :: B.Board -> B.Color -> Int
getFlatCount board color =
  length $ filter (isFlat) (M.controlledPositions board color)
  where
    isFlat :: B.Position -> Bool
    isFlat (B.Position (x, y)) = B.ps (head (getElem x y board)) == B.Flat

getPrisonerCount :: B.Board -> B.Color -> Int
getPrisonerCount board color = sum $ map foo cps
  where
    cps = M.controlledPositions board color
    foo (B.Position (x, y)) =
      length $ (filter ((/= color) . B.pc) (getElem x y board))

getReserves :: B.Board -> B.Color -> Int
getReserves board color = sum $ map foo cps
  where
    cps = M.controlledPositions board color
    foo (B.Position (x, y)) =
      length $ (filter ((== color) . B.pc) (getElem x y board))

getBuddies :: B.Board -> B.Color -> [B.Position] -> Int
getBuddies board color positions = sum $ map foo positions
  where
    foo :: B.Position -> Int
    foo p = length $ filter bar (B.getNeighbors p board)
    bar :: B.Position -> Bool
    bar (B.Position (x, y))
      | getElem x y board == [] = False
      | otherwise = B.pc (head (getElem x y board)) == color

longestRoad :: B.Board -> B.Color -> Int
longestRoad board color = maximum $ (rows n n [0]) ++ (cols n n [0])
  where
    n = ncols board
    rows :: Int -> Int -> [Int] -> [Int]
    rows _ _ [] = []
    rows 0 _ xs = xs
    rows x 0 xs = rows (x - 1) n (0 : xs)
    rows x y (c:xs)
      | getElem x y board == [] = rows (x - 1) y (c : xs)
      | (B.pc . head) (getElem x y board) == color = rows x (y - 1) (c + 1 : xs)
      | otherwise = rows x (y - 1) (c - 1 : xs)
    cols :: Int -> Int -> [Int] -> [Int]
    cols _ _ [] = []
    cols _ 0 xs = xs
    cols 0 y xs = cols n (y - 1) (0 : xs)
    cols x y (c:xs)
      | getElem x y board == [] = cols x (y - 1) (c : xs)
      | (B.pc . head) (getElem x y board) == color = cols (x - 1) y (c + 1 : xs)
      | otherwise = cols (x - 1) y (c - 1 : xs)

encourageCenterPlay :: B.Board -> [B.Position] -> Int
encourageCenterPlay board positions = sum $ map foo positions
  where
    center = 3
    center2 = 3
    foo :: B.Position -> Int
    foo (B.Position (x, y)) = -xoff - yoff
      where
        xoff = max (abs (x - center)) (abs (x - center2))
        yoff = max (abs (y - center)) (abs (y - center2))

winnablePositions :: B.Board -> B.Color -> [B.Position] -> Int
winnablePositions board color positions = sum $ map foo positions
  where
    foo :: B.Position -> Int
    foo (B.Position (x, y)) =
      bar (B.getNeighbors (B.Position (x, y)) board) (head (getElem x y board))
    bar :: [B.Position] -> B.Piece -> Int
    bar [] _ = 0
    bar _ (B.Piece _ B.Cap) = 12 + (sum $ map sscore positions)
    bar ns (B.Piece _ B.Standing) = 7 + (sum $ map sscore ns)
    bar ns _ = sum $ map fscore ns
    sscore :: B.Position -> Int
    sscore (B.Position (x, y))
      | null (getElem x y board) = -1
      | B.pc (head (getElem x y board)) == color = 1
      | B.ps (head (getElem x y board)) == B.Flat = 2
      | otherwise = 0
    fscore :: B.Position -> Int
    fscore (B.Position (x, y))
      | null (getElem x y board) = 0
      | B.pc (head (getElem x y board)) == color = 1
      | B.ps (head (getElem x y board)) == B.Flat = -1
      | B.ps (head (getElem x y board)) == B.Cap = -5
      | B.ps (head (getElem x y board)) == B.Standing = -2
      | otherwise = 0
