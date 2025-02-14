module EvalMut where

import qualified Board as B
import Control.Monad (filterM, foldM, forM, forM_, unless, when)
import Data.IORef
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM
import qualified Moves as M
import qualified MutableState as MS

roadWin :: Int
roadWin = 1000000000

flatWin :: Int
flatWin = roadWin - 1

bestEval' :: MS.MGameState s -> IO Int
bestEval' ms = do
  score <- checkForWinScore ms
  case score of
    Just score -> return score
    _ -> do
      let b = MS.mBoard ms
      whiteControlled <- MS.controlledPositions b B.White
      blackControlled <- MS.controlledPositions b B.Black
      whiteScore <- evaluateControlled b whiteControlled B.White
      blackScore <- evaluateControlled b blackControlled B.Black
      wb <- getBuddies b B.White whiteControlled
      bb <- getBuddies b B.Black blackControlled
      lrw <- longestRoad b B.White
      lrb <- longestRoad b B.Black
      wpw <- winnablePositions b B.White
      wpb <- winnablePositions b B.Black
      let ws = whiteScore + (13 * wb) + (17 * lrw) + (4 * wpw)
      let bs = blackScore + (13 * bb) + (17 * lrb) + (4 * wpb)
      return $ ws - bs

-------------------------- 
-- | Helper Functions | --
--------------------------
checkForWinScore :: MS.MGameState s -> IO (Maybe Int)
checkForWinScore mgs = do
  r <- MS.checkGameResult mgs
  case r of
    B.Road B.White -> return $ Just roadWin
    B.Road B.Black -> return $ Just (-roadWin)
    B.FlatWin B.White -> return $ Just flatWin
    B.FlatWin B.Black -> return $ Just (-flatWin)
    _ -> return Nothing

evaluateControlled :: MS.MBoard s -> [B.Position] -> B.Color -> IO Int
evaluateControlled b ps c = do
  foldM
    (\acc p -> do
       ep <- evaluatePosition b p c
       return $ acc + ep)
    0
    ps

evaluatePosition :: MS.MBoard s -> B.Position -> B.Color -> IO Int
evaluatePosition b p@(B.Position (x, y)) c = do
  score <- newIORef 0
  square <- MS.readSquare b p
  let top = head square
  when (B.ps top == B.Flat) $ modifyIORef score (+ 100)
  let xoff = (x - 3) * (x - 4)
  let yoff = (y - 3) * (y - 4)
  let maxoff = max xoff yoff
  modifyIORef score (+ (-3 * maxoff))
  when (B.ps top == B.Cap) $ modifyIORef score (+ (-17 * maxoff))
  let prisoners = length $ filter ((/= c) . B.pc) square
  if B.ps top == B.Standing
    then modifyIORef score (+ (7 * prisoners))
    else if B.ps top == B.Cap
           then modifyIORef score (+ (15 * prisoners))
           else modifyIORef score (+ (2 * prisoners))
  let reserves = length $ filter ((== c) . B.pc) square
  if B.ps top == B.Standing
    then modifyIORef score (+ (10 * reserves))
    else if B.ps top == B.Cap
           then modifyIORef score (+ (22 * reserves))
           else modifyIORef score (+ (2 * reserves))
  readIORef score

getBuddies :: MS.MBoard s -> B.Color -> [B.Position] -> IO Int
getBuddies b c ps = sum <$> mapM (getBuddy b c) ps

getBuddy :: MS.MBoard s -> B.Color -> B.Position -> IO Int
getBuddy b c p@(B.Position (x, y)) = do
  let n = MS.boardSize b
  let possibleNeighbors =
        [ B.Position (x + 1, y)
        , B.Position (x - 1, y)
        , B.Position (x, y + 1)
        , B.Position (x, y - 1)
        ]
  neighbors <- filterM (isBuddy n) possibleNeighbors
  return $ length neighbors
  where
    isBuddy :: Int -> B.Position -> IO Bool
    isBuddy n p@(B.Position (x', y')) = do
      if x' < 1 || x' > n || y' < 1 || y' > n
        then return False
        else do
          square <- MS.readSquare b p
          if null square
            then return False
            else return $ B.pc (head square) == c

longestRoad :: MS.MBoard s -> B.Color -> IO Int
longestRoad b c = do
  let n = MS.boardSize b
  visited <- VM.replicate (n * n) False
  let positions = map (MS.indexToPos b) [0 .. (n * n - 1)]
  foldM
    (\acc p -> do
       len <- roadDFS b c p visited
       return $ max acc len)
    0
    positions

roadDFS :: MS.MBoard s -> B.Color -> B.Position -> IOVector Bool -> IO Int
roadDFS b c p visited = do
  let idx = MS.posToIndex b p
  alreadyVisited <- VM.read visited idx
  if alreadyVisited
    then return 0
    else do
      VM.write visited idx True
      square <- MS.readSquare b p
      if null square || B.pc (head square) /= c
        then return 0
        else do
          let top = head square
          let pieceScore =
                case B.ps top of
                  B.Standing -> 3
                  B.Cap -> 10
                  _ -> 0
          neighbors <- getNeighbors b p
          validNeighbors <- filterM foo neighbors
          lengths <- mapM (\n -> roadDFS b c n visited) validNeighbors
          return $ (1 + pieceScore) + maximum (0 : lengths)
  where
    foo :: B.Position -> IO Bool
    foo p' = do
      square <- MS.readSquare b p'
      return $ not (null square) && B.pc (head square) == c

winnablePositions :: MS.MBoard s -> B.Color -> IO Int
winnablePositions b c = sum <$> mapM (winnablePosition b c) ps
  where
    n = MS.boardSize b
    ps = map (MS.indexToPos b) [0 .. (n * n - 1)]

winnablePosition :: MS.MBoard s -> B.Color -> B.Position -> IO Int
winnablePosition b c p = do
  square <- MS.readSquare b p
  if null square
    then return 0
    else do
      let top = head square
      let score =
            if B.ps top == B.Cap
              then 12
              else if B.ps top == B.Standing
                     then 7
                     else 0
      neighbors <- getNeighbors b p
      nsScores <- mapM (evaluateNeighbor b c) neighbors
      return $ score + sum nsScores

evaluateNeighbor :: MS.MBoard s -> B.Color -> B.Position -> IO Int
evaluateNeighbor b c p = do
  square <- MS.readSquare b p
  if null square
    then return (-1)
    else do
      let top = head square
      return $
        case B.ps top of
          B.Flat ->
            if B.pc top == c
              then 2
              else -1
          B.Cap -> -5
          B.Standing -> -2
          _ -> 0

getNeighbors :: MS.MBoard s -> B.Position -> IO [B.Position]
getNeighbors b p@(B.Position (x, y)) =
  let possibleNeighbors =
        [ B.Position (x + 1, y)
        , B.Position (x - 1, y)
        , B.Position (x, y + 1)
        , B.Position (x, y - 1)
        ]
   in filterM (isNeighbor b) possibleNeighbors
  where
    isNeighbor :: MS.MBoard s -> B.Position -> IO Bool
    isNeighbor b p@(B.Position (x', y')) = do
      let n = MS.boardSize b
      if x' < 1 || x' > n || y' < 1 || y' > n
        then return False
        else return True
