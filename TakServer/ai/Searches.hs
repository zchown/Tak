{-# LANGUAGE BangPatterns #-}

module Searches where

import qualified Board as B
import Control.Monad (filterM, foldM, forM, forM_, unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List
import Data.Maybe
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM
import Eval
import qualified EvalMut as EM
import qualified Moves as M
import qualified MutableState as MS

negaMax :: (B.GameState -> Int) -> B.GameState -> Int -> Maybe B.Move
negaMax eval gs depth
  | depth == 0 = Nothing
  | null moves = Nothing
  | otherwise =
    let !bestMove =
          foldl'
            (\acc@(score, _) m ->
               let !score' = negaMax' (M.doMove gs m) (depth - 1) c
                in if score' > score
                     then (score', Just m)
                     else acc)
            (-roadWin, Nothing)
            moves
     in snd bestMove
  where
    !moves = M.generateAllMoves gs
    c =
      case B.turn gs of
        B.White -> 1
        B.Black -> -1
    negaMax' :: B.GameState -> Int -> Int -> Int
    negaMax' gs' depth' color
      | depth' == 0 = color * eval gs'
      | isJust r = (fromJust r) * color
      | otherwise =
        foldl'
          (\alpha m ->
             max alpha (-negaMax' (M.doMove gs' m) (depth' - 1) (-color)))
          (-roadWin)
          moves
      where
        !moves = M.generateAllMoves gs'
        !r = checkForWinScore gs'

alphaBeta :: (B.GameState -> Int) -> B.GameState -> Int -> Maybe B.Move
alphaBeta eval gs depth
  | depth == 0 = Nothing
  | null moves = Nothing
  | otherwise =
    let !bestMove =
          foldl'
            (\acc@(score, _) m ->
               let newScore =
                     alphabeta' (M.doMove gs m) (depth - 1) c (-roadWin) roadWin
                in if newScore > score
                     then (newScore, Just m)
                     else acc)
            (-roadWin, Nothing)
            moves
     in snd bestMove
  where
    !moves = M.generateAllMoves gs
    c =
      case B.turn gs of
        B.White -> 1
        B.Black -> -1
    alphabeta' :: B.GameState -> Int -> Int -> Int -> Int -> Int
    alphabeta' gs' depth' color alpha beta
      | depth' == 0 = color * eval gs'
      | isJust r = (fromJust r) * color
      | otherwise =
        foldl'
          (\a m ->
             if a >= beta
               then a
               else max
                      a
                      (-alphabeta'
                         (M.doMove gs' m)
                         (depth' - 1)
                         (-color)
                         (-beta)
                         (-a)))
          alpha
          moves
      where
        !r = checkForWinScore gs'
        !moves = M.generateAllMoves gs'

negaMaxMut ::
     (MS.MGameState s -> IO Int) -> MS.MGameState s -> Int -> IO (Maybe B.Move)
negaMaxMut eval mgs depth
  | depth == 0 = return Nothing
  | otherwise = do
    moves <- MS.generateAllMoves mgs
    if VM.length moves == 0
      then return Nothing
      else do
        color <- readIORef (MS.mTurn mgs)
        let c =
              case color of
                B.White -> 1
                B.Black -> -1
        bestMove <-
          foldM
            (\acc@(score, _) i -> do
               m <- VM.read moves i
               MS.makeMoveNoCheck (MS.mBoard mgs) m
               score' <- negaMax' eval mgs (depth - 1) c
               MS.undoMoveNoChecks (MS.mBoard mgs) m
               if score' > score
                 then return (score', Just m)
                 else return acc)
            (-roadWin, Nothing)
            [0 .. VM.length moves - 1]
        return $ snd bestMove
  where
    negaMax' ::
         (MS.MGameState s -> IO Int) -> MS.MGameState s -> Int -> Int -> IO Int
    negaMax' eval' mgs' depth' color
      | depth' == 0 = do
        score <- eval' mgs'
        return $ color * score
      | otherwise = do
        moves <- MS.generateAllMoves mgs'
        if VM.length moves == 0
          then return 0
          else do
            r <- EM.checkForWinScore mgs'
            if isJust r
              then return $ (fromJust r) * color
              else do
                bestScore <- newIORef (-roadWin)
                forM_ [0 .. VM.length moves - 1] $ \i -> do
                  m <- VM.read moves i
                  alpha <- readIORef bestScore
                  if alpha >= roadWin
                    then return ()
                    else do
                      MS.makeMoveNoCheck (MS.mBoard mgs') m
                      score <- negaMax' eval' mgs' (depth' - 1) (-color)
                      MS.undoMoveNoChecks (MS.mBoard mgs') m
                      bestScore' <- readIORef bestScore
                      writeIORef bestScore $ max bestScore' (-score)
                readIORef bestScore

alphaBetaMut ::
     (MS.MGameState s -> IO Int) -> MS.MGameState s -> Int -> IO (Maybe B.Move)
alphaBetaMut eval mgs depth
  | depth == 0 = return Nothing
  | otherwise = do
    moves <- MS.generateAllMoves mgs
    if VM.length moves == 0
      then return Nothing
      else do
        color <- readIORef (MS.mTurn mgs)
        let c =
              if color == B.White
                then 1
                else -1
        bestMoveRef <- newIORef (Nothing, -roadWin)
        let alphaBeta' alpha beta i =
              when (i < VM.length moves) $ do
                m <- VM.read moves i
                MS.makeMoveNoCheck (MS.mBoard mgs) m
                score <-
                  fmap negate $
                  alphabeta' eval mgs (depth - 1) (-beta) (-alpha) (-c)
                MS.undoMoveNoChecks (MS.mBoard mgs) m
                (bestMove, bestScore) <- readIORef bestMoveRef
                when (score > bestScore) $
                  writeIORef bestMoveRef (Just m, score)
                let newAlpha = max alpha score
                unless (newAlpha >= beta) $ alphaBeta' newAlpha beta (i + 1)
        alphaBeta' (-roadWin) roadWin 0
        fst <$> readIORef bestMoveRef
  where
    alphabeta' eval' mgs' depth' alpha beta color
      | depth' == 0 = fmap (* color) (eval' mgs')
      | otherwise = do
        moves <- MS.generateAllMoves mgs'
        if VM.length moves == 0
          then return 0
          else do
            r <- EM.checkForWinScore mgs'
            if isJust r
              then return $ (fromJust r) * color
              else do
                bestScoreRef <- newIORef (-roadWin)
                let search i alpha'
                      | i >= VM.length moves || alpha' >= beta = return ()
                      | otherwise = do
                        m <- VM.read moves i
                        MS.makeMoveNoCheck (MS.mBoard mgs') m
                        score <-
                          fmap negate $
                          alphabeta'
                            eval'
                            mgs'
                            (depth' - 1)
                            (-beta)
                            (-alpha')
                            (-color)
                        MS.undoMoveNoChecks (MS.mBoard mgs') m
                        bestScore <- readIORef bestScoreRef
                        writeIORef bestScoreRef (max bestScore score)
                        search (i + 1) (max alpha' score)
                search 0 alpha
                readIORef bestScoreRef
