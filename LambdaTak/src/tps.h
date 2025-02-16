#ifndef TPS_H
#define TPS_H

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#include "board.h"

GameState* parseTPS(const char* tps);
char* boardToTPS(Board* board);
char* gameStateToTPS(GameState* state);

#endif // TPS_H

