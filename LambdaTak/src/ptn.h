#ifndef PTN_H
#define PTN_H

#include "board.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    char* site;
    char* event;
    char* date;
    char* time;
    char* player1;
    char* player2;
    char* clock;
    char* result;
    int size;
    GameHistory* moves;
} PTN;

typedef enum {
    PTN_PARSE_OK,
    PTN_PARSE_ERROR,
    PTN_DIRECTION_ERROR,
    PTN_COUNT_ERROR,
    PTN_MOVE_ERROR,
    PTN_POSITION_ERROR,
    PTN_COLOR_ERROR,
    PTN_STONE_ERROR,
    PTN_SLIDE_ERROR
} PTNParseError;

// Function prototypes
PTNParseError parsePTN(const char* input, PTN* ptn);
PTNParseError parseMove(const char* moveStr, Color color, Move* move);
PTNParseError parseSlideMove(const char* str, Color color, bool crush, Move* move);
Direction charToDirection(char c);
char directionToChar(Direction dir);
void freePTN(PTN* ptn);

#endif // PTN_H
