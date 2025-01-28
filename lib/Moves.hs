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
checkPlace b (B.Position row col)
  | row < 1 || col < 1 = Left $ InvalidMove "Invalid Position (1, 1) or greater"
  | row > nrows b || col > ncols b =
    Left $ InvalidMove "Invalid Position (nrows, ncols) or less"
  | not $ null $ getElem row col b = Left $ InvalidMove "Square Occupied"
  | otherwise = Right True

checkSlide :: B.Board -> B.Move -> Either InvalidMove Bool
checkSlide _ (B.PlaceFlat _) = Left $ InvalidMove "Can't Slide a PlaceFlat"
checkSlide _ (B.PlaceStanding _) =
  Left $ InvalidMove "Can't Slide a PlaceStanding"
checkSlide _ (B.PlaceCap _) = Left $ InvalidMove "Can't Slide a PlaceCap"
checkSlide b (B.Slide (pos@(B.Position row col), count, dir, drops, color, crush))
  | row < 1 || col < 1 = Left $ InvalidMove "Invalid Position (1, 1) or greater"
  | row > nrows b || col > ncols b =
    Left $ InvalidMove "Invalid Position (nrows, ncols) or less"
  | null drops = Left $ InvalidMove "No Drops"
  | td /= count || count > ncols b = Left $ InvalidMove "Invalid Count or Drops"
  | any (< 1) drops = Left $ InvalidMove "Invalid Drop"
  | count < 1 || count > length (getElem row col b) =
    Left $ InvalidMove "Invalid Count For Stack"
  | dir == B.Up && row - dl < 1 = Left $ InvalidMove "Not Enough Rows Up"
  | dir == B.Down && row + dl > nrows b =
    Left $ InvalidMove "Not Enough Rows Down"
  | dir == B.Left && col - dl < 1 = Left $ InvalidMove "Not Enough Columns Left"
  | dir == B.Right && col + dl > ncols b =
    Left $ InvalidMove "Not Enough Columns Right"
  | color /= B.pc (head $ getElem row col b) =
    Left $ InvalidMove "Color Does Not Control Stack"
  | checkForCap b pos dir dl = Left $ InvalidMove "Cap In The Way"
  | checkForStanding b pos dir dl lps = Left $ InvalidMove "Standing In The Way"
  | crush && checkForCrush b pos dir ld lps =
    Left $ InvalidMove "Crush not set correctly"
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
    (newPos, row', col') = B.getNextPos (B.Position row col) dir

checkForStanding ::
     B.Board -> B.Position -> B.Direction -> Int -> [B.Piece] -> Bool
checkForStanding _ _ _ 0 _ = False
checkForStanding b (B.Position row col) dir 1 ps
  | null (getElem row' col' b) = False
  | topStanding b newPos = length ps == 1 && lastCap ps
  | otherwise = False
  where
    (newPos, row', col') = B.getNextPos (B.Position row col) dir
checkForStanding b (B.Position row col) dir dl ps
  | null (getElem row' col' b) = checkForStanding b newPos dir (dl - 1) ps
  | topStanding b (B.Position (row - 1) col) = True
  | otherwise = checkForStanding b newPos dir (dl - 1) ps
  where
    (newPos, row', col') = B.getNextPos (B.Position row col) dir

checkForCrush ::
     B.Board -> B.Position -> B.Direction -> Int -> [B.Piece] -> Bool
checkForCrush _ _ _ _ [] = False
checkForCrush b (B.Position row col) dir dl [B.Piece _ B.Cap]
  | null (getElem (row - dl) col b) = False
  | topStanding b newPos = True
  | otherwise = False
  where
    (newPos, _, _) = B.getNextPos (B.Position row col) dir
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
    ps = reverse $ take count $ getElem row col b
    s' = drop count $ getElem row col b
    b' = setElem s' (row, col) b
    (pos', _, _) = B.getNextPos (B.Position row col) dir

makeSlide ::
     B.Board
  -> B.Direction
  -> B.Position
  -> [B.Piece]
  -> [Int]
  -> B.Crush
  -> B.Board
makeSlide b _ _ _ [] _ = b
makeSlide b _ (B.Position row col) [x] _ True =
  let s = getElem row col b
      s' = drop 1 s
      h' =
        case B.pc (head s) of
          B.Black -> B.Piece B.Black B.Flat
          B.White -> B.Piece B.White B.Flat
   in setElem (x : h' : s') (row, col) b
makeSlide b dir (B.Position row col) xs (d:ds) crush =
  let s = getElem row col b
      dp = reverse $ take d xs
      s' = dp ++ s
      b' = setElem s' (row, col) b
      xs' = drop d xs
   in makeSlide b' dir nextPos xs' ds crush
  where
    (nextPos, _, _) = B.getNextPos (B.Position row col) dir

data InvalidUndo
  = InvalidPlaceUndo
  | InvalidSlideUndo
  | InvalidUndoPosition
  deriving (Show, Eq)

undoMove :: B.Board -> B.Move -> Either InvalidUndo B.Board
undoMove b (B.PlaceFlat (pos, _)) = undoPlaceMove b pos
undoMove b (B.PlaceStanding (pos, _)) = undoPlaceMove b pos
undoMove b (B.PlaceCap (pos, _)) = undoPlaceMove b pos
undoMove b (B.Slide (pos@(B.Position row col), count, dir, drops, _, _))
  | sum drops /= count = Left InvalidSlideUndo
  | count < 1 || count > ncols b = Left InvalidSlideUndo
  | checkLength pos count dir = Left InvalidSlideUndo
  | otherwise = undoSlide b newPos dir drops []
  where
    checkLength :: B.Position -> Int -> B.Direction -> Bool
    checkLength (B.Position r _) n B.Up = r - n >= 1
    checkLength (B.Position r _) n B.Down = r + n <= nrows b
    checkLength (B.Position _ c) n B.Left = c - n >= 1
    checkLength (B.Position _ c) n B.Right = c + n <= ncols b
    newPos =
      case dir of
        B.Up -> B.Position (row - length drops) col
        B.Down -> B.Position (row + length drops) col
        B.Left -> B.Position row (col - length drops)
        B.Right -> B.Position row (col + length drops)

undoPlaceMove :: B.Board -> B.Position -> Either InvalidUndo B.Board
undoPlaceMove b (B.Position row col)
  | row < 1 || col < 1 || row > nrows b || col > ncols b =
    Left InvalidUndoPosition
  | null (getElem row col b) = Left InvalidPlaceUndo
  | length (getElem row col b) > 1 = Left InvalidPlaceUndo
  | otherwise = Right $ setElem [] (row, col) b

undoSlide ::
     B.Board
  -> B.Position
  -> B.Direction
  -> [Int]
  -> [B.Piece]
  -> Either InvalidUndo B.Board
undoSlide b (B.Position row col) _ [] xs =
  Right $ setElem (reverse xs) (row, col) b
undoSlide b (B.Position row col) dir (d:ds) xs
  | d < 1 = Left InvalidSlideUndo
  | otherwise = do
    let s = getElem row col b
    if length s < d
      then Left InvalidSlideUndo
      else do
        let (piecesToMove, remaining) = splitAt d s
            newXs = piecesToMove ++ xs
            b' = setElem remaining (row, col) b
            (nextPos, _, _) = B.getNextPos (B.Position row col) dir
        undoSlide b' nextPos dir ds newXs

-------------------------
-- | Move Generation | --
-------------------------
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
  [ B.Position row col
  | row <- [1 .. nrows board]
  , col <- [1 .. ncols board]
  , null (getElem row col board)
  ]

controlledPositions :: B.Board -> B.Color -> [B.Position]
controlledPositions board color =
  [ B.Position row col
  | row <- [1 .. nrows board]
  , col <- [1 .. ncols board]
  , not (null (getElem row col board))
  , B.pc (head (getElem row col board)) == color
  ]

generateSlidesForPosition :: B.Board -> B.Color -> B.Position -> [B.Move]
generateSlidesForPosition board color pos =
  concatMap
    (generateSlidesInDirection board color pos)
    [B.Up, B.Down, B.Left, B.Right]

generateSlidesInDirection ::
     B.Board -> B.Color -> B.Position -> B.Direction -> [B.Move]
generateSlidesInDirection board color pos@(B.Position row col) dir =
  [ B.Slide (pos, count, dir, drops, color, canCrush board pos dir drops)
  | count <- [1 .. maxCount]
  , drops <- validDrops count
  , isValidSlide board pos count dir drops color (canCrush board pos dir drops)
  ]
  where
    maxCount = length (getElem row col board)
    steps = numSteps dir pos board
    validDrops count =
      [ ds
      | ds <- dropSequences steps count
      , sum ds == count
      , all (> 0) ds
      , isValidDropSequence ds count
      ]

isValidDropSequence :: [Int] -> Int -> Bool
isValidDropSequence drops initialCount =
  let remainingAfterEachDrop = scanl (-) initialCount drops
   in all (>= 0) remainingAfterEachDrop

dropSequences :: Int -> Int -> [[Int]]
dropSequences steps count = go steps count []
  where
    go :: Int -> Int -> [Int] -> [[Int]]
    go 0 0 acc = [reverse acc]
    go 0 _ _ = []
    go stepsLeft remaining acc
      | remaining < 0 = []
      | stepsLeft == 1 =
        if remaining > 0
          then [reverse (remaining : acc)]
          else []
      | otherwise =
        concatMap
          (\i -> go (stepsLeft - 1) (remaining - i) (i : acc))
          [0 .. remaining]

canCrush :: B.Board -> B.Position -> B.Direction -> [Int] -> Bool
canCrush board startPos dir drops =
  let movingStack = getElem row col board
      endPos = getFinalPosition startPos dir (length drops)
      targetSquare =
        case endPos of
          B.Position r c
            | r > 0 && r <= nrows board && c > 0 && c <= ncols board ->
              getElem r c board
          _ -> []
   in case (movingStack, targetSquare, drops) of
        (stack, target, ds)
          | not (null stack) &&
              B.ps (head stack) == B.Cap &&
              last ds == 1 &&
              not (null target) && B.ps (head target) == B.Standing -> True
        _ -> False
  where
    (B.Position row col) = startPos

getFinalPosition :: B.Position -> B.Direction -> Int -> B.Position
getFinalPosition (B.Position r c) dir steps =
  case dir of
    B.Up -> B.Position (r - steps) c
    B.Down -> B.Position (r + steps) c
    B.Left -> B.Position r (c - steps)
    B.Right -> B.Position r (c + steps)

numSteps :: B.Direction -> B.Position -> B.Board -> Int
numSteps B.Up (B.Position row _) _ = row - 1
numSteps B.Down (B.Position row _) b = nrows b - row
numSteps B.Left (B.Position _ col) _ = col - 1
numSteps B.Right (B.Position _ col) b = ncols b - col

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
