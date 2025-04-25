#ifndef MOVES_H
#define MOVES_H

#include "board.h"
#include "tps.h"

typedef enum {SUCCESS, INVALID_MOVE, INVALID_POSITION, INVALID_DIRECTION, INVALID_STONE, INVALID_COLOR, INVALID_MOVE_TYPE, INVALID_COUNT, INVALID_CRUSH, INVALID_SLIDE, INVALID_DROPS} MoveResult;

typedef struct {
    Move* moves;
    u32 numMoves;
    int capacity;
} MoveList;


MoveResult checkMove(GameState* state, const Move* move);

MoveResult makeMoveChecks(GameState* state, const Move* move);
GameState* makeMoveNoChecks(GameState* state, const Move* move, bool doHistory);

MoveResult undoMoveChecks(GameState* state, const Move* move);
GameState* undoMoveNoChecks(GameState* state, const Move* move, bool doHistory);

int generateAllMoves(const GameState* state, MoveList* moveList);
int generateSlidesInDir(const GameState* state, Position pos, Direction dir, MoveList* moveList);

int countAllMoves(const GameState* state);

// Utility functions
u8 numSteps(const GameState* state, Position pos, Direction dir);

MoveList* createMoveList(u32 numMoves);
void freeMoveList(MoveList* moves);
void addMoveToList(MoveList* moveList, Move move);
void clearMoveList(MoveList* moveList);

#endif // MOVES_H
