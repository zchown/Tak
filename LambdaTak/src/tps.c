#include "tps.h"

GameState* parseTPS(const char* tps) {
    GameState* state = createGameState();
    if (state == NULL) {
        printf("Failed to create game state\n");
        return NULL;
    }

    // 256 probably is fine should be more then enough
    // 8 is way more then enough for the move number
    char boardStr[256], turnStr[2], moveNumStr[8];
    if (strncmp(tps, "[TPS ", 4) == 0) {
        // read until the first space, then the turn, then the move number
        sscanf(tps, "[TPS %255[^ ] %1s %7[^]]", boardStr, turnStr, moveNumStr);
    }
    else {
        sscanf(tps, "%255[^ ] %1s %7[^]]", boardStr, turnStr, moveNumStr);
    }

    state->turn = turnStr[0] == '1' ? WHITE : BLACK;
    state->turnNumber = atoi(moveNumStr);

    char* rows[BOARD_SIZE];
    int rowCount = 0;

    // savePtr is used to maintain the state of strtok_r
    char* savePtr;
    char* curRow = strtok_r(boardStr, "/", &savePtr);

    while(curRow && rowCount < BOARD_SIZE) {
        rows[rowCount++] = curRow;
        curRow = strtok_r(NULL, "/", &savePtr);
    }

    if (rowCount != BOARD_SIZE) {
        printf("Invalid number of rows in TPS\n");
        freeGameState(state);
        return NULL;
    }

    u8 rowNumber = BOARD_SIZE - 1;
    for (u32 i = 0; i < BOARD_SIZE; i++) {
        rowNumber -= i;
        curRow = rows[i];
        char* savePtr2;
        char* curToken = strtok_r(curRow, ",", &savePtr2);
        u8 colNumber = 0;

        while(curToken && colNumber < BOARD_SIZE) {
            if (curToken[0] == 'x') {
                u8 count = 1;
                if (strlen(curToken) > 1) {
                    count = (u8)strtol(curToken + sizeof(char), NULL, 10);
                }
                if (count <= 0 || count > BOARD_SIZE) {
                    count = 1;
                }
                // don't have to do anything as squares are already empty
                colNumber += count;
            }
            else {
                Position pos = {colNumber, rowNumber};
                Square* sq = &state->board->squares[positionToIndex(pos)];
                Piece* lastPiece = NULL;
                sq->head = lastPiece;
                for (u32 j = 0; j < strlen(curToken); j++) {
                    char c = curToken[j];
                    if (c == '1' || c == '2') {
                        Color color = c == '1' ? WHITE : BLACK;
                        Stone stone = FLAT;
                        Piece* newPiece = createPiece(stone, color);
                        if (newPiece == NULL) {
                            printf("Failed to create piece\n");
                            freeGameState(state);
                            return NULL;
                        }
                        newPiece->next = lastPiece;
                        sq->head = newPiece;
                        sq->numPieces++;
                        lastPiece = newPiece;
                    }
                    else {
                        Stone stone = c == 'S' ? STANDING : CAP;
                        sq->head->stone = stone;
                    }
                    colNumber++;
                }
            }
            curToken = strtok_r(NULL, ",", &savePtr2);
        }
        if (colNumber != BOARD_SIZE) {
            printf("Invalid number of columns in TPS\n");
            freeGameState(state);
            return NULL;
        }
    }
    updateReserves(state);
    return state;
}

char* boardToTPS(Board* board) {
    char* boardStr = malloc(256);
    if (boardStr == NULL) {
        printf("Failed to allocate memory for TPS\n");
        return NULL;
    }

    u32 i = 0;
    for (u8 row = 0; row < BOARD_SIZE; row++) {
        if (row != 0) {
            boardStr[i++] = '/';
        }
        u8 xs = 0; // used to count the number of empty squares
        for (u8 col = 0; col < BOARD_SIZE; col++) {
            Square* sq = &board->squares[positionToIndex((Position){col, row})];
            if (sq->numPieces == 0) {
                xs++;
            }
            else {
                if (xs > 0) {
                    boardStr[i++] = 'x';
                    if (xs > 1) {
                        boardStr[i++] = '0' + xs;
                    }
                    xs = 0;
                }
                Piece* curPiece = sq->head;
                while (curPiece) {
                    boardStr[i++] = curPiece->color == WHITE ? '1' : '2';
                    if (curPiece->stone != FLAT) {
                        boardStr[i++] = curPiece->stone == CAP ? 'C' : 'S';
                    }
                    curPiece = curPiece->next;
                }
            }
            if (col == BOARD_SIZE - 1 && xs > 0) {
                if (xs > 1) {
                    boardStr[i++] = '0' + xs;
                }
                boardStr[i++] = 'x';
            }
            if (col != BOARD_SIZE - 1) {
                boardStr[i++] = ',';
            }
        }
        if (xs > 0) {
            boardStr[i++] = 'x';
            if (xs > 1) {
                boardStr[i++] = '0' + xs;
            }
        }
    }
    boardStr[i++] = '\0';
    boardStr = realloc(boardStr, i);
    return boardStr;
}

char* gameStateToTPS(GameState* state) {
    char* boardStr = boardToTPS(state->board);
    if (boardStr == NULL) {
        printf("Failed to convert board to TPS\n");
        return NULL;
    }

    char* tps = malloc(256);
    if (tps == NULL) {
        printf("Failed to allocate memory for TPS\n");
        free(boardStr);
        return NULL;
    }
    int turn = state->turn == WHITE ? 1 : 2;
    sprintf(tps, "[TPS %s %d %llu]", boardStr, turn, state->turnNumber);
    free(boardStr);
    return tps;
}
