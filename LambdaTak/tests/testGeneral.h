#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <dirent.h>
#include <string.h>
#include "../lib/board.h"
#include "../lib/moves.h"
#include "../lib/ptn.h"
#include "../lib/tps.h"

bool areEqualBoards(const GameState* b1, const GameState* b2);

int countMoves(const GameHistory* history);

Move* historyToMoveArray(const GameHistory* history, int* moveCount);

void validateMoves(GameState* board, Move* moves, int moveCount);

char** getPTNFiles(const char* dirPath, int* fileCount);

Move* readPTNFile(const char* filePath, int* moveCount);

bool areEqualBoards(const GameState* b1, const GameState* b2);

void runGeneralTests(void);

