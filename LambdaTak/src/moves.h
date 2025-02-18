#ifndef MOVES_H
#define MOVES_H

#include "board.h"

typedef enum {SUCCESS, INVALID_MOVE, INVALID_POSITION, INVALID_DIRECTION, INVALID_STONE, INVALID_COLOR, INVALID_MOVE_TYPE, INVALID_COUNT, INVALID_CRUSH, INVALID_SLIDE, INVALID_DROPS} MoveResult;

MoveResult checkMove(GameState* state, Move* move);

GameState* makeMoveChecks(GameState* state, Move* move);
GameState* makeMoveNoChecks(GameState* state, Move* move);

GameState* undoMoveChecks(GameState* state);
GameState* undoMoveNoChecks(GameState* state);

Move* generateAllMoves(GameState* state);

// Utility functions
u32** dropSequence(u32 count, u32 spaces);
void generateCombination(u32 n, u32 k, u32 idx, u32* comb);
u32 binomialCoefficient(u32 n, u32 k);
u32 countValidSequences(u32 count, u32 spaces);
u32** dropSequencesForCrush(u32 count, u32 spaces);
u8 numSteps(GameState* state, Position pos, Direction dir);


#endif // MOVES_H
