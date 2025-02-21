#ifndef MOVES_H
#define MOVES_H

#include "board.h"

typedef enum {SUCCESS, INVALID_MOVE, INVALID_POSITION, INVALID_DIRECTION, INVALID_STONE, INVALID_COLOR, INVALID_MOVE_TYPE, INVALID_COUNT, INVALID_CRUSH, INVALID_SLIDE, INVALID_DROPS} MoveResult;

typedef struct {
    Move* moves;
    u32 numMoves;
} GeneratedMoves;


MoveResult checkMove(GameState* state, const Move* move);

MoveResult makeMoveChecks(GameState* state, const Move* move);
GameState* makeMoveNoChecks(GameState* state, const Move* move);

MoveResult undoMoveChecks(GameState* state, const Move* move);
GameState* undoMoveNoChecks(GameState* state, const Move* move);

GeneratedMoves* generateAllMoves(const GameState* state);
void generateSlidesInDir(const GameState* state, Position pos, Direction dir, Move* moves, u32* totalMoves);

// Utility functions
u16* dropSequence(u8 count, u8 spaces);
u8 binomialCoefficient(u8 n, u8 k);
u8 countValidSequences(u8 count, u8 spaces);
u16* dropSequencesForCrush(u8 count, u8 spaces);
u8 numSteps(const GameState* state, Position pos, Direction dir);
void freeGeneratedMoves(GeneratedMoves* moves);

#endif // MOVES_H
