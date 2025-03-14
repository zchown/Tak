{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module MutableState where

import qualified Board as B
import Control.Monad (filterM, foldM, forM, forM_, unless, when)
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

toGameState :: MGameState s -> IO B.GameState
toGameState mgs = do
  board <- toBoard (mBoard mgs)
  turn <- readIORef (mTurn mgs)
  moveNumber <- readIORef (mMoveNumber mgs)
  player1 <- readIORef (mPlayer1 mgs)
  player2 <- readIORef (mPlayer2 mgs)
  result <- readIORef (mResult mgs)
  history <- readIORef (mGameHistory mgs)
  return $ B.GameState board turn moveNumber player1 player2 result history

fromGameState :: B.GameState -> IO (MGameState s)
fromGameState gs = do
  board <- createMutableBoard (B.board gs)
  turn <- newIORef (B.turn gs)
  moveNumber <- newIORef (B.moveNumber gs)
  player1 <- newIORef (B.player1 gs)
  player2 <- newIORef (B.player2 gs)
  result <- newIORef (B.result gs)
  history <- newIORef (B.gameHistory gs)
  return $ MGameState board turn moveNumber player1 player2 result history

createMutableBoard :: B.Board -> IO (MBoard s)
createMutableBoard b = do
  VM.generateM ((length squares)) (\i -> return (squares !! i))
  where
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
posToIndex b (B.Position (x, y)) = (y - 1) + (x - 1) * (boardSize b)

indexToPos :: MBoard s -> Int -> B.Position
indexToPos b i = B.Position (x, y)
  where
    y = (i `mod` boardSize b) + 1
    x = (i `div` boardSize b) + 1

readSquare :: MBoard s -> B.Position -> IO B.Square
readSquare b p = VM.read b (posToIndex b p)

writeSquare :: MBoard s -> B.Position -> B.Square -> IO ()
writeSquare b p = VM.write b (posToIndex b p)

toBoard :: MBoard s -> IO B.Board
toBoard b = do
  let size = boardSize b
  squares <-
    forM [0 .. (size * size) - 1] $ \i -> do
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
  let (pos', _, _) = B.getSlidePos p dir (length drops)
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
      -- putStrLn $ "Placements: " ++ show placements
      c <- readIORef (mTurn gs)
      slides <- generateSlides (mBoard gs) c
      -- putStrLn $ "Slides: " ++ show slides
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
        if c == B.White
          then mPlayer1 gs
          else mPlayer2 gs
  r <- readIORef reservesRef
  emptyPos <- emptyPositions (mBoard gs)
  -- putStrLn $ "Empty positions: " ++ show (length emptyPos)
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
      drops = dropSequences slideLength maxCount
  noCrushes <-
    forM drops $ \d -> do
      let count = sum d
      return $ B.Slide (p, count, dir, d, c, False)
  if edgeDist == slideLength
    then return noCrushes
    else do
      let (crushPos, _, _) = B.getSlidePos p dir (slideLength + 1)
      csq <- readSquare b crushPos
      if null csq || B.ps (head csq) /= B.Standing || B.ps (head s) /= B.Cap
        then return noCrushes
        else do
          let crushDrops = dropSequencesForCrush slideLength (maxCount - 1)
          crushes <-
            forM crushDrops $ \d -> do
              let count = sum d
              return $ B.Slide (p, count, dir, d, c, True)
          return (noCrushes ++ crushes)

checkGameResult :: MGameState s -> IO B.Result
checkGameResult gs = do
  let b = mBoard gs
  c <- readIORef (mTurn gs)
  f <- checkGameWin b c
  case f of
    B.Continue -> do
      p1 <- readIORef (mPlayer1 gs)
      p2 <- readIORef (mPlayer2 gs)
      let rd = B.checkReservesDraw p1 p2
      case rd of
        Nothing -> do
          f' <- checkFullBoard b True
          case f' of
            B.Continue -> return B.Draw
            x -> return x
        _ -> checkFullBoard b False
    r -> return r

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
  deriving (Eq, Show)

checkGameWin :: MBoard s -> B.Color -> IO B.Result
checkGameWin b c = do
  let c' = B.flipColor c
  roadCVert <- checkRoadWin b c Vertical
  roadCHoriz <- checkRoadWin b c Horizontal
  roadICVert <- checkRoadWin b c' Vertical
  roadICHoriz <- checkRoadWin b c' Horizontal
  return $
    case () of
      _ -- laziness + short-circuiting is killer here
        | roadCVert -> B.Road c
        | roadCHoriz -> B.Road c
        | roadICVert -> B.Road c'
        | roadICHoriz -> B.Road c'
        | otherwise -> B.Continue

checkRoadWin :: MBoard s -> B.Color -> Direction -> IO Bool
checkRoadWin b c dir = do
  result <- newIORef False
  forM_ [1 .. boardSize b] $ \i -> do
    currentResult <- readIORef result
    unless currentResult $ do
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

findRoadWin :: Direction -> MBoard s -> B.Color -> B.Position -> IO Bool
findRoadWin dir b c p = go [p] []
  where
    n = boardSize b
    go :: [B.Position] -> [B.Position] -> IO Bool
    go [] _ = return False
    go (p'@(B.Position (i, j)):stack) visited
      | (dir == Vertical && j == n) || (dir == Horizontal && i == n) =
        return True
      | otherwise = do
        valid <- validNeighbors dir b p' c visited
        -- putStrLn $ "Valid neighbors: " ++ show valid
        -- putStrLn $ "Dir: " ++ show dir
        let newStack = valid ++ stack
        if (dir == Vertical && B.Position (i, n) `elem` valid) ||
           (dir == Horizontal && B.Position (n, j) `elem` valid)
            -- putStrLn "Found road win"
          then do
            return True
          else go newStack (p : visited ++ valid)

--------------------------
-- | Helper Functions | --
--------------------------
validNeighbors ::
     Direction
  -> MBoard s
  -> B.Position
  -> B.Color
  -> [B.Position]
  -> IO [B.Position]
validNeighbors dir b (B.Position (x, y)) c visited = do
  let possibleNeighbors =
        case dir -- use dir to try to optimnize search
              of
          Vertical ->
            [ B.Position (x, y + 1) -- up
            , B.Position (x + 1, y) -- right
            , B.Position (x - 1, y) -- left
            , B.Position (x, y - 1) -- down
            ]
          Horizontal ->
            [ B.Position (x + 1, y) -- right
            , B.Position (x, y + 1) -- up
            , B.Position (x, y - 1) -- down
            , B.Position (x - 1, y) -- left
            ]
  filterM isValidNeighbor possibleNeighbors
  where
    isValidNeighbor :: B.Position -> IO Bool
    isValidNeighbor p'@(B.Position (x', y')) = do
      let v = visited
      if x' < 1 || y' < 1 || x' > boardSize b || y' > boardSize b || p' `elem` v
        then do
          return False
        else do
          s <- readSquare b p'
          return $
            not (null s) && B.pc (head s) == c && B.ps (head s) /= B.Standing

slideToEnd :: MBoard s -> B.Position -> B.Direction -> Int
slideToEnd b (B.Position (_, y)) B.Up = (boardSize b - y)
slideToEnd _ (B.Position (_, y)) B.Down = y - 1
slideToEnd _ (B.Position (x, _)) B.Left = x - 1
slideToEnd b (B.Position (x, _)) B.Right = (boardSize b - x)

emptyPositions :: MBoard s -> IO [B.Position]
emptyPositions b = do
  let size = boardSize b
  foldM
    (\acc i -> do
       let p = indexToPos b i
       s <- readSquare b p
       return $
         if null s
           then p : acc
           else acc)
    []
    [0 .. size * size - 1]

controlledPositions :: MBoard s -> B.Color -> IO [B.Position]
controlledPositions b c = do
  let size = boardSize b
  foldM
    (\acc i -> do
       let p = indexToPos b i
       s <- readSquare b p
       return $
         if not (null s) && B.pc (head s) == c
           then p : acc
           else acc)
    []
    [0 .. size * size - 1]

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

dropSequences :: Int -> Int -> [[Int]]
dropSequences s c = go c
  where
    go :: Int -> [[Int]]
    go 0 = []
    go n = Moves.dropSequences s n ++ go (n - 1)
