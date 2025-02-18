#ifndef MOVES_H
#define MOVES_H

#include "board.h"

typedef enum {SUCCESS, INVALID_MOVE, INVALID_POSITION, INVALID_DIRECTION, INVALID_STONE, INVALID_COLOR, INVALID_MOVE_TYPE, INVALID_COUNT, INVALID_CRUSH, INVALID_SLIDE, INVALID_DROPS} MoveResult;

MoveResult checkMove(GameState* state, const Move* move);

MoveResult makeMoveChecks(GameState* state, const Move* move);
GameState* makeMoveNoChecks(GameState* state, const Move* move);

MoveResult undoMoveChecks(GameState* state, const Move* move);
GameState* undoMoveNoChecks(GameState* state, const Move* move);

Move* generateAllMoves(const GameState* state);
void generateSlidesInDir(const GameState* state, Position pos, Direction dir, Move* moves, u32* totalMoves);

// Utility functions
u32** dropSequence(u32 count, u32 spaces);
u32 binomialCoefficient(u32 n, u32 k);
u32 countValidSequences(u32 count, u32 spaces);
u32** dropSequencesForCrush(u32 count, u32 spaces);
u8 numSteps(GameState* state, Position pos, Direction dir);


#endif // MOVES_H
