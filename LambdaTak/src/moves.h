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
u8* dropSequence(u8 count, u8 spaces, u8* drops);
u8* dropSequencesForCrush(u8 count, u8 spaces, u8* drops);
u8* numSteps(GameState* state, Position pos, Direction dir);

#endif // MOVES_H
