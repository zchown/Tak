#ifndef TPS_H
#define TPS_H

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#include "board.h"

GameState* parseTPS(const char* tps);
char* gameStateToTPS(GameState* state);
char* boardToTPS(Board* board);

#endif // TPS_H

