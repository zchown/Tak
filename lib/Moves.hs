module Moves where

import qualified Board as B
import Data.List (nub)
import Data.Matrix

newtype InvalidMove =
  InvalidMove String
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
checkPlace b (B.Position (x, y))
  | x < 1 || y < 1 = Left $ InvalidMove "Invalid Position (1, 1) or greater"
  | y > nrows b || x > ncols b =
    Left $ InvalidMove "Invalid Position (nrows, ncols) or less"
  | not $ null $ getElem x y b = Left $ InvalidMove "Square Occupied"
  | otherwise = Right True

checkSlide :: B.Board -> B.Move -> Either InvalidMove Bool
checkSlide _ (B.PlaceFlat _) = Left $ InvalidMove "Can't Slide a PlaceFlat"
checkSlide _ (B.PlaceStanding _) =
  Left $ InvalidMove "Can't Slide a PlaceStanding"
checkSlide _ (B.PlaceCap _) = Left $ InvalidMove "Can't Slide a PlaceCap"
checkSlide b (B.Slide (pos@(B.Position (x, y)), count, dir, drops, color, crush))
  | x < 1 || y < 1 = Left $ InvalidMove "Invalid Position (1, 1) or greater"
  | y > nrows b || x > ncols b =
    Left $ InvalidMove "Invalid Position (nrows, ncols) or less"
  | null drops = Left $ InvalidMove "No Drops"
  | td /= count || count > ncols b = Left $ InvalidMove "Invalid Count or Drops"
  | any (< 1) drops = Left $ InvalidMove "Invalid Drop"
  | count < 1 || count > length (getElem x y b) =
    Left $ InvalidMove "Invalid Count For Stack"
  | dir == B.Up && y + dl > nrows b = Left $ InvalidMove "Not Enough Space Up"
  | dir == B.Down && y - dl < 1 = Left $ InvalidMove "Not Enough Space Down"
  | dir == B.Left && x - dl < 1 = Left $ InvalidMove "Not Enough Space Left"
  | dir == B.Right && x + dl > ncols b =
    Left $ InvalidMove "Not Enough Space Right"
  | color /= B.pc (head $ getElem x y b) =
    Left $ InvalidMove "Color Does Not Control Stack"
  | checkForCap b pos dir dl = Left $ InvalidMove "Cap In The Way"
  | checkForStanding b newPos dir (dl - 1) standingBool =
    Left $ InvalidMove "Standing In The Way"
  | cc /= crush = Left $ InvalidMove "Crush not set correctly"
  | otherwise = Right True
  where
    dl = length drops
    td = sum drops
    ld = last drops
    lc =
      if null (getElem x y b)
        then False
        else B.ps (head (getElem x y b)) == B.Cap
    standingBool = ld == 1 && lc
    cc = checkForCrush b pos dir dl lc
    (newPos, _, _) = B.getNextPos pos dir

checkForCap :: B.Board -> B.Position -> B.Direction -> Int -> Bool
checkForCap _ _ _ 0 = False
checkForCap b p dir dl
  | null (getElem x' y' b) = checkForCap b newPos dir (dl - 1)
  | B.ps (head $ getElem x' y' b) == B.Cap = True
  | otherwise = checkForCap b newPos dir (dl - 1)
  where
    (newPos, x', y') = B.getNextPos p dir

checkForStanding :: B.Board -> B.Position -> B.Direction -> Int -> Bool -> Bool
checkForStanding b pos@(B.Position (x, y)) _ 0 lc
  | lc = False
  | null (getElem x y b) = False
  | topStanding b pos = True
  | otherwise = False
checkForStanding b pos@(B.Position (x, y)) dir dl ps
  | null (getElem x y b) = checkForStanding b newPos dir (dl - 1) ps
  | topStanding b pos = True
  | otherwise = checkForStanding b newPos dir (dl - 1) ps
  where
    (newPos, _, _) = B.getNextPos pos dir

checkForCrush :: B.Board -> B.Position -> B.Direction -> Int -> Bool -> Bool
checkForCrush _ _ _ _ False = False
checkForCrush b p dir dl True
  | null (getElem x' y' b) = False
  | otherwise = topStanding b newPos
  where
    (newPos, x', y') = B.getSlidePos p dir dl

topStanding :: B.Board -> B.Position -> Bool
topStanding b (B.Position (x, y))
  | null (getElem x y b) = False
  | otherwise = B.ps (head $ getElem x y b) == B.Standing

lastCap :: [B.Piece] -> Bool
lastCap [] = False
lastCap ps = B.ps (last ps) == B.Cap

-------------------------------
-- -- | Make and Undo Move | --
-- ----------------------------
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
makeMove b m@(B.Slide (p@(B.Position (x, y)), count, dir, drops, _, crush)) =
  case checkMove b m of
    Left e -> Left e
    Right _ -> Right $ makeSlide b' dir pos' ps drops crush
  where
    ps = reverse $ take count $ getElem x y b
    s' = drop count $ getElem x y b
    b' = setElem s' (x, y) b
    (pos', _, _) = B.getNextPos p dir

makeSlide ::
     B.Board
  -> B.Direction
  -> B.Position
  -> [B.Piece]
  -> [Int]
  -> B.Crush
  -> B.Board
makeSlide b _ _ _ [] _ = b
makeSlide b _ (B.Position (x, y)) [pc] _ True =
  let s = getElem x y b
      s' = drop 1 s
      h' =
        case B.pc (head s) of
          B.Black -> B.Piece B.Black B.Flat
          B.White -> B.Piece B.White B.Flat
   in setElem (pc : h' : s') (x, y) b
makeSlide b dir p@(B.Position (x, y)) xs (d:ds) crush =
  let s = getElem x y b
      dp = reverse $ take d xs
      s' = dp ++ s
      b' = setElem s' (x, y) b
      xs' = drop d xs
   in makeSlide b' dir nextPos xs' ds crush
  where
    (nextPos, _, _) = B.getNextPos p dir

data InvalidUndo
  = InvalidPlaceUndo String
  | InvalidSlideUndo String
  | InvalidUndoPosition String
  deriving (Show, Eq)

undoMove :: B.Board -> B.Move -> Either InvalidUndo B.Board
undoMove b (B.PlaceFlat (pos, _)) = undoPlaceMove b pos
undoMove b (B.PlaceStanding (pos, _)) = undoPlaceMove b pos
undoMove b (B.PlaceCap (pos, _)) = undoPlaceMove b pos
undoMove b (B.Slide (pos, count, dir, drops, _, _))
  | sum drops /= count = Left $ InvalidSlideUndo "Drop Count Mismatch"
  | count < 1 || count > ncols b = Left $ InvalidSlideUndo "Invalid Count"
  | not $ checkLength pos (length drops) dir =
    Left $ InvalidSlideUndo "Invalid Length"
  | otherwise = undoSlide b newPos dir drops []
  where
    checkLength :: B.Position -> Int -> B.Direction -> Bool
    checkLength (B.Position (_, r)) n B.Up = r + n <= nrows b
    checkLength (B.Position (_, r)) n B.Down = r - n >= 1
    checkLength (B.Position (c, _)) n B.Left = c - n >= 1
    checkLength (B.Position (c, _)) n B.Right = c + n <= ncols b
    (newPos, _, _) = B.getSlidePos pos dir (length drops)

undoPlaceMove :: B.Board -> B.Position -> Either InvalidUndo B.Board
undoPlaceMove b (B.Position (x, y))
  | x < 1 || y < 1 || y > nrows b || x > ncols b =
    Left $ InvalidUndoPosition "Invalid Position"
  | null (getElem x y b) = Left $ InvalidPlaceUndo "Square Empty"
  | length (getElem x y b) > 1 = Left $ InvalidPlaceUndo "Stack Too Big"
  | otherwise = Right $ setElem [] (x, y) b

undoSlide ::
     B.Board
  -> B.Position
  -> B.Direction
  -> [Int]
  -> [B.Piece]
  -> Either InvalidUndo B.Board
undoSlide b (B.Position (x, y)) _ [] xs = Right $ setElem (reverse xs) (x, y) b
undoSlide b p@(B.Position (x, y)) dir (d:ds) xs
  | d < 1 = Left $ InvalidSlideUndo "Invalid Drop"
  | otherwise = do
    let s = getElem x y b
    if length s < d
      then Left $ InvalidSlideUndo "Not Enough Pieces"
      else do
        let (piecesToMove, remaining) = splitAt d s
            newXs = piecesToMove ++ xs
            b' = setElem remaining (x, y) b
            (nextPos, _, _) = B.getNextPos p $ B.getInverseDir dir
        undoSlide b' nextPos dir ds newXs

-- -------------------------
-- -- | Move Generation | --
-- -------------------------
generateAllMoves :: B.GameState -> [B.Move]
generateAllMoves gs
  | B.moveNumber gs == 1 = firstMovePlacement gs
  | otherwise = nub $ placementMoves gs ++ slideMoves (B.board gs) (B.turn gs)

-- First move must place opponent's flat stone on any empty square
firstMovePlacement :: B.GameState -> [B.Move]
firstMovePlacement gs =
  [B.PlaceFlat (pos, oppositeColor) | pos <- emptyPositions (B.board gs)]
  where
    oppositeColor =
      if B.turn gs == B.White
        then B.Black
        else B.White

placementMoves :: B.GameState -> [B.Move]
placementMoves gs =
  let reserves =
        if B.turn gs == B.White
          then B.player1 gs
          else B.player2 gs
      emptyPos = emptyPositions (B.board gs)
   in concat
        [ [ B.PlaceFlat (pos, B.turn gs)
          | B.stones reserves > 0
          , pos <- emptyPos
          ]
        , [ B.PlaceStanding (pos, B.turn gs)
          | B.stones reserves > 0
          , pos <- emptyPos
          ]
        , [B.PlaceCap (pos, B.turn gs) | B.caps reserves > 0, pos <- emptyPos]
        ]

slideMoves :: B.Board -> B.Color -> [B.Move]
slideMoves board color =
  concatMap
    (generateSlidesForPosition board color)
    (controlledPositions board color)

emptyPositions :: B.Board -> [B.Position]
emptyPositions board =
  [ B.Position (x, y)
  | x <- [1 .. nrows board]
  , y <- [1 .. ncols board]
  , null (getElem x y board)
  ]

controlledPositions :: B.Board -> B.Color -> [B.Position]
controlledPositions board color =
  [ B.Position (x, y)
  | x <- [1 .. nrows board]
  , y <- [1 .. ncols board]
  , not (null (getElem x y board))
  , B.pc (head (getElem x y board)) == color
  ]

generateSlidesForPosition :: B.Board -> B.Color -> B.Position -> [B.Move]
generateSlidesForPosition board color pos =
  concatMap
    (generateSlidesInDirection board color pos)
    [B.Up, B.Down, B.Left, B.Right]

generateSlidesInDirection ::
     B.Board -> B.Color -> B.Position -> B.Direction -> [B.Move]
generateSlidesInDirection board color pos@(B.Position (x, y)) dir =
  [ B.Slide (pos, count, dir, drops, color, canCrush board pos dir drops)
  | count <- [1 .. maxCount]
  , drops <- validDrops count
  , isValidSlide board pos count dir drops color (canCrush board pos dir drops)
  ]
  where
    maxCount = length (getElem x y board)
    steps = numSteps dir pos board
    validDrops count =
      [ds | ds <- dropSequences steps count, sum ds == count, all (> 0) ds]

dropSequences :: Int -> Int -> [[Int]]
dropSequences steps count = go steps count []
  where
    go :: Int -> Int -> [Int] -> [[Int]]
    go _ 0 acc = [acc]
    go 0 _ _ = []
    go sl remaining acc
      | remaining < 1 = []
      | sl < 1 = []
      | otherwise =
        [ res
        | i <- [1 .. remaining]
        , res <- go (sl - 1) (remaining - i) (i : acc)
        ]

canCrush :: B.Board -> B.Position -> B.Direction -> [Int] -> Bool
canCrush board startPos@(B.Position (x, y)) dir drops =
  let movingStack = getElem x y board
      (endPos, _, _) = B.getSlidePos startPos dir (length drops)
      targetSquare =
        case endPos of
          B.Position (x', y')
            | x' > 0 && x' <= nrows board && y' > 0 && y' <= ncols board ->
              getElem x' y' board
          _ -> []
   in case (movingStack, targetSquare, drops) of
        (stack, target, ds)
          | not (null stack) &&
              B.ps (head stack) == B.Cap &&
              last ds == 1 &&
              not (null target) && B.ps (head target) == B.Standing -> True
        _ -> False

numSteps :: B.Direction -> B.Position -> B.Board -> Int
numSteps B.Up (B.Position (x, _)) b = nrows b - x
numSteps B.Down (B.Position (x, _)) _ = x - 1
numSteps B.Left (B.Position (_, y)) _ = y - 1
numSteps B.Right (B.Position (_, y)) b = ncols b - y

isValidSlide ::
     B.Board
  -> B.Position
  -> Int
  -> B.Direction
  -> [Int]
  -> B.Color
  -> Bool
  -> Bool
isValidSlide board pos count dir drops color crush =
  case checkSlide board (B.Slide (pos, count, dir, drops, color, crush)) of
    Right _ -> True
    Left _ -> False
