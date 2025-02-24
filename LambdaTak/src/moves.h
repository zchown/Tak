#ifndef MOVES_H
#define MOVES_H

#include "board.h"
#include "tps.h"

typedef enum {SUCCESS, INVALID_MOVE, INVALID_POSITION, INVALID_DIRECTION, INVALID_STONE, INVALID_COLOR, INVALID_MOVE_TYPE, INVALID_COUNT, INVALID_CRUSH, INVALID_SLIDE, INVALID_DROPS} MoveResult;

typedef struct {
    Move* moves;
    u32 numMoves;
} GeneratedMoves;


MoveResult checkMove(GameState* state, const Move* move);

MoveResult makeMoveChecks(GameState* state, const Move* move);
GameState* makeMoveNoChecks(GameState* state, const Move* move, bool doHistory);

MoveResult undoMoveChecks(GameState* state, const Move* move);
GameState* undoMoveNoChecks(GameState* state, const Move* move, bool doHistory);

GeneratedMoves* generateAllMoves(const GameState* state, u32 prevMoves);
void generateSlidesInDir(const GameState* state, Position pos, Direction dir, Move* moves, u32* totalMoves);

// Utility functions
u8 numSteps(const GameState* state, Position pos, Direction dir);
void freeGeneratedMoves(GeneratedMoves* moves);

#endif // MOVES_H
