{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module MutableState where

import qualified Board as B
import Control.Monad (foldM, forM, forM_, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Matrix as M
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM
import qualified Moves

type MBoard s = IOVector B.Square

data MGameState s = MGameState
  { mBoard :: MBoard s
  , mTurn :: IORef B.Color
  , mMoveNumber :: IORef Int
  , mPlayer1 :: IORef B.Reserves
  , mPlayer2 :: IORef B.Reserves
  , mResult :: IORef B.Result
  , mGameHistory :: IORef B.History
  }

createMutableBoard :: B.Board -> IO (MBoard s)
createMutableBoard b = do
  mv <- VM.new (rows * cols)
  forM_ [0 .. (rows * cols - 1)] $ \i -> do
    VM.write mv i (squares !! i)
  return mv
  where
    rows = M.nrows b
    cols = M.ncols b
    squares = M.toList b

boardSize :: MBoard s -> Int
boardSize b
  | VM.length b == 16 = 4
  | VM.length b == 25 = 5
  | VM.length b == 36 = 6
  | VM.length b == 49 = 7
  | VM.length b == 64 = 8
  | otherwise = 0

posToIndex :: MBoard s -> B.Position -> Int
posToIndex b (B.Position (x, y)) = x + y * boardSize b

indexToPos :: MBoard s -> Int -> B.Position
indexToPos b i = B.Position (i `mod` boardSize b, i `div` boardSize b)

readSquare :: MBoard s -> B.Position -> IO B.Square
readSquare b p = VM.read b (posToIndex b p)

writeSquare :: MBoard s -> B.Position -> B.Square -> IO ()
writeSquare b p = VM.write b (posToIndex b p)

toBoard :: MBoard s -> IO B.Board
toBoard b = do
  let size = boardSize b
  squares <-
    forM [0 .. (size * size - 1)] $ \i -> do
      readSquare b (indexToPos b i)
  return $ M.fromList size size squares

checkMove :: MBoard s -> B.Move -> IO (Either Moves.InvalidMove Bool)
checkMove b' m = do
  b <- toBoard b'
  return $ Moves.checkMove b m

makeMove :: MBoard s -> B.Move -> IO (Either Moves.InvalidMove ())
makeMove b m@(B.PlaceFlat (pos, c)) = do
  cm <- checkMove b m
  case cm of
    Left e -> return $ Left e
    Right _ -> do
      writeSquare b pos [B.Piece c B.Flat]
      return $ Right ()
makeMove b m@(B.PlaceStanding (pos, c)) = do
  cm <- checkMove b m
  case cm of
    Left e -> return $ Left e
    Right _ -> do
      writeSquare b pos [B.Piece c B.Standing]
      return $ Right ()
makeMove b m@(B.PlaceCap (pos, c)) = do
  cm <- checkMove b m
  case cm of
    Left e -> return $ Left e
    Right _ -> do
      writeSquare b pos [B.Piece c B.Cap]
      return $ Right ()
makeMove b m@(B.Slide (p, count, dir, drops, _, crush)) = do
  cm <- checkMove b m
  case cm of
    Left e -> return $ Left e
    Right _ -> do
      xs <- readSquare b p
      let ps = reverse $ take count xs
      let s' = drop count xs
      writeSquare b p s'
      let (pos', _, _) = B.getNextPos p dir
      makeSlide b dir pos' ps drops crush
      return $ Right ()

makeMoveNoCheck :: MBoard s -> B.Move -> IO ()
makeMoveNoCheck b (B.PlaceFlat (pos, c)) = do
  writeSquare b pos [B.Piece c B.Flat]
makeMoveNoCheck b (B.PlaceStanding (pos, c)) = do
  writeSquare b pos [B.Piece c B.Standing]
makeMoveNoCheck b (B.PlaceCap (pos, c)) = do
  writeSquare b pos [B.Piece c B.Cap]
makeMoveNoCheck b (B.Slide (p, count, dir, drops, _, crush)) = do
  xs <- readSquare b p
  let ps = reverse $ take count xs
  let s' = drop count xs
  writeSquare b p s'
  let (pos', _, _) = B.getNextPos p dir
  makeSlide b dir pos' ps drops crush

makeSlide ::
     MBoard s
  -> B.Direction
  -> B.Position
  -> [B.Piece]
  -> [Int]
  -> B.Crush
  -> IO ()
makeSlide _ _ _ _ [] _ = return ()
makeSlide b _ p [pc] _ True = do
  s <- readSquare b p
  let s' = drop 1 s
  let h' =
        case B.pc (head s) of
          B.Black -> B.Piece B.Black B.Flat
          B.White -> B.Piece B.White B.Flat
  writeSquare b p (pc : h' : s')
  return ()
makeSlide b dir p xs (d:ds) crush = do
  s <- readSquare b p
  let dp = reverse $ take d xs
  let s' = dp ++ s
  writeSquare b p s'
  let xs' = drop d xs
  let (pos', _, _) = B.getNextPos p dir
  makeSlide b dir pos' xs' ds crush

undoMoveNoChecks :: MBoard s -> B.Move -> IO ()
undoMoveNoChecks b (B.Slide (p, _, dir, drops, _, crush)) = do
  let (pos', _, _) = B.getNextPos p dir
  if crush
    then do
      undoCrush b pos'
      undoSlide b pos' dir (reverse drops) []
    else do
      undoSlide b pos' dir (reverse drops) []
undoMoveNoChecks b m = writeSquare b (B.getPosition m) []

undoCrush :: MBoard s -> B.Position -> IO ()
undoCrush brd p = do
  s <- readSquare brd p
  let s' =
        case s of
          (a:b:cs) -> a : B.flipStanding b : cs
          as -> as
  writeSquare brd p s'

undoSlide ::
     MBoard s -> B.Position -> B.Direction -> [Int] -> [B.Piece] -> IO ()
undoSlide b p _ [] xs = do
  s <- readSquare b p
  writeSquare b p (xs ++ s)
undoSlide b p dir (d:ds) xs = do
  s <- readSquare b p
  let (piecesToMove, remaining) = splitAt d s
      newXs = xs ++ piecesToMove
  writeSquare b p remaining
  let (pos', _, _) = B.getNextPos p $ B.getInverseDir dir
  undoSlide b pos' dir ds newXs

generateAllMoves :: MGameState s -> IO (IOVector B.Move)
generateAllMoves gs = do
  mn <- readIORef (mMoveNumber gs)
  if mn <= 2
    then do
      moves <- firstMovePlacement gs
      vec <- VM.new (length moves)
      mapM_ (\(i, m) -> VM.write vec i m) (zip [0 ..] moves)
      return vec
    else do
      placements <- generatePlacements gs
      c <- readIORef (mTurn gs)
      slides <- generateSlides (mBoard gs) c
      let allMoves = placements ++ slides
      vec <- VM.new (length allMoves)
      mapM_ (\(i, m) -> VM.write vec i m) (zip [0 ..] allMoves)
      return vec

firstMovePlacement :: MGameState s -> IO [B.Move]
firstMovePlacement gs = do
  c <- readIORef (mTurn gs)
  let c' = B.flipColor c
  map (B.PlaceFlat . (, c')) <$> emptyPositions (mBoard gs)

generatePlacements :: MGameState s -> IO [B.Move]
generatePlacements gs = do
  c <- readIORef (mTurn gs)
  let reservesRef =
        if c == B.Black
          then mPlayer1 gs
          else mPlayer2 gs
  r <- readIORef reservesRef
  emptyPos <- emptyPositions (mBoard gs)
  return $
    concat
      [ [B.PlaceFlat (pos, c) | B.stones r > 0, pos <- emptyPos]
      , [B.PlaceStanding (pos, c) | B.stones r > 0, pos <- emptyPos]
      , [B.PlaceCap (pos, c) | B.caps r > 0, pos <- emptyPos]
      ]

generateSlides :: MBoard s -> B.Color -> IO [B.Move]
generateSlides b c = do
  controlledPos <- controlledPositions b c
  concat <$> mapM (generateSlidesFromPos b c) controlledPos

generateSlidesFromPos :: MBoard s -> B.Color -> B.Position -> IO [B.Move]
generateSlidesFromPos b c p =
  concat <$> mapM (generateSlidesFromDir b c p) [B.Up, B.Down, B.Left, B.Right]

generateSlidesFromDir ::
     MBoard s -> B.Color -> B.Position -> B.Direction -> IO [B.Move]
generateSlidesFromDir b c p dir = do
  s <- readSquare b p
  let size = boardSize b
      maxCount = min size (length s)
  slideLength <- numSteps dir p b
  let edgeDist = slideToEnd b p dir
      drops = Moves.dropSequences slideLength maxCount
  noCrushes <-
    forM drops $ \d -> do
      let count = sum d
      return $ B.Slide (p, count, dir, d, c, False)
  if edgeDist == slideLength
    then return noCrushes
    else do
      let (crushPos, _, _) = B.getSlidePos p dir (slideLength + 1)
      csq <- readSquare b crushPos
      if null csq || B.ps (head csq) /= B.Standing
        then return noCrushes
        else do
          let crushDrops = dropSequencesForCrush slideLength (maxCount - 1)
          crushes <-
            forM crushDrops $ \d -> do
              let count = sum d
              return $ B.Slide (p, count, dir, d, c, True)
          return (noCrushes ++ crushes)

checkFullBoard :: MBoard s -> Bool -> IO B.Result
checkFullBoard b f = go 1 1 (0, 0)
  where
    n = boardSize b
    go :: Int -> Int -> (Int, Int) -> IO B.Result
    go x y c@(wc, bc)
      | x > n = go 1 (y + 1) c
      | y > n =
        return $
        if wc > bc
          then B.FlatWin B.White
          else if bc > wc
                 then B.FlatWin B.Black
                 else B.Draw
      | otherwise = do
        square <- readSquare b (B.Position (x, y))
        if not f && null square
          then return B.Continue
          else go (x + 1) y (addCount c square)
    addCount :: (Int, Int) -> B.Square -> (Int, Int)
    addCount (w', b') square
      | not (null square) =
        case B.pc (head square) of
          B.White -> (w' + 1, b')
          B.Black -> (w', b' + 1)
      | otherwise = (w', b')

data Direction
  = Vertical
  | Horizontal

checkRoadWin :: MBoard s -> B.Color -> Direction -> IO Bool
checkRoadWin b c dir = do
  result <- newIORef False
  forM_ [1 .. boardSize b] $ \i -> do
    currentResult <- readIORef result
    when (not currentResult) $ do
      let p =
            case dir of
              Vertical -> B.Position (i, 1)
              Horizontal -> B.Position (1, i)
      square <- readSquare b p
      when
        (not (null square) &&
         B.pc (head square) == c && B.ps (head square) /= B.Standing) $ do
        win <- findRoadWin dir b c p
        when win $ writeIORef result True
  readIORef result

--------------------------
-- | Helper Functions | --
--------------------------
slideToEnd :: MBoard s -> B.Position -> B.Direction -> Int
slideToEnd b (B.Position (_, y)) B.Up = (boardSize b - y) - 1
slideToEnd _ (B.Position (_, y)) B.Down = y - 1
slideToEnd _ (B.Position (x, _)) B.Left = x - 1
slideToEnd b (B.Position (x, _)) B.Right = (boardSize b - x) - 1

emptyPositions :: MBoard s -> IO [B.Position]
emptyPositions b = foldM foo [] [0 .. (VM.length b - 1)]
  where
    foo :: [B.Position] -> Int -> IO [B.Position]
    foo acc i = do
      let p = indexToPos b i
      s <- readSquare b p
      return $
        if null s
          then p : acc
          else acc

controlledPositions :: MBoard s -> B.Color -> IO [B.Position]
controlledPositions b c = foldM foo [] [0 .. (VM.length b - 1)]
  where
    foo :: [B.Position] -> Int -> IO [B.Position]
    foo acc i = do
      let p = indexToPos b i
      s <- readSquare b p
      return $
        if B.controlledBy s c
          then p : acc
          else acc

numSteps :: B.Direction -> B.Position -> MBoard s -> IO Int
numSteps dir p b = go 0 (fst3 (B.getNextPos p dir))
  where
    n = boardSize b
    fst3 (x, _, _) = x
    go :: Int -> B.Position -> IO Int
    go acc pos@(B.Position (x, y)) = do
      let notInBounds = x < 1 || y < 1 || x > n || y > n
      if notInBounds
        then return acc
        else do
          s <- readSquare b pos
          if null s
            then go (acc + 1) (fst3 (B.getNextPos pos dir))
            else do
              if B.ps (head s) /= B.Flat
                then return acc
                else go (acc + 1) (fst3 (B.getNextPos pos dir))

dropSequencesForCrush :: Int -> Int -> [[Int]]
dropSequencesForCrush s c = map (++ [1]) ds
  where
    ds = filter (\x -> length x == s) (Moves.dropSequences s c)

canCrush :: MBoard s -> B.Position -> B.Direction -> [Int] -> IO Bool
canCrush board startPos@(B.Position (x, y)) dir drops = do
  movingStack <- readSquare board startPos
  let (endPos, x', y') = B.getSlidePos startPos dir (length drops)
  targetSquare <-
    if (x' > 0 && x' < boardSize board && y' > 0 && y' < boardSize board)
      then readSquare board endPos
      else return []
  return $
    not (null movingStack) &&
    (B.ps (head movingStack) == B.Cap) &&
    last drops == 1 &&
    not (null targetSquare && (B.ps (head targetSquare) == B.Standing))
