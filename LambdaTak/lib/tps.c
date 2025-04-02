#include "tps.h"

GameState* parseTPS(const char* tps) {
    GameState* state = createGameState();
    if (state == NULL) {
        printf("Failed to create game state\n");
        return NULL;
    }

    char boardStr[256], turnStr[2], moveNumStr[8];
    int scanned;
    if (strncmp(tps, "[TPS ", 5) == 0) {
        scanned = sscanf(tps, "[TPS %255[^ ] %1s %7[^]]", boardStr, turnStr, moveNumStr);
    }
    else {
        scanned = sscanf(tps, "%255[^ ] %1s %7[^]]", boardStr, turnStr, moveNumStr);
    }
    if (scanned != 3) {
        printf("TPS string format error\n");
        freeGameState(state);
        return NULL;
    }

    state->turn = (turnStr[0] == '1') ? WHITE : BLACK;
    state->turnNumber = atoi(moveNumStr);

    // Split boardStr into rows using '/' as delimiter.
    char* rows[BOARD_SIZE];
    int rowCount = 0;
    char* savePtr;
    char* curRow = strtok_r(boardStr, "/", &savePtr);
    while (curRow && rowCount < BOARD_SIZE) {
        rows[rowCount++] = curRow;
        curRow = strtok_r(NULL, "/", &savePtr);
    }
    if (rowCount != BOARD_SIZE) {
        printf("Invalid number of rows in TPS\n");
        printf("Expected %d, got %d\n", BOARD_SIZE, rowCount);
        printf("TPS: %s\n", boardStr);
        freeGameState(state);
        return NULL;
    }

    for (u32 rowNum = 0; rowNum < BOARD_SIZE; rowNum++) {
        curRow = rows[rowNum];

        char* savePtr2;
        char* curToken = strtok_r(curRow, ",", &savePtr2);
        u8 colNumber = 0;

        while (curToken && colNumber < BOARD_SIZE) {
            if (curToken[0] == 'x') {
                // Token denotes a run of empty squares.
                u8 count = 1;
                if (strlen(curToken) > 1) {
                    count = (u8)strtol(curToken + 1, NULL, 10);
                }
                if (count <= 0 || count > BOARD_SIZE) {
                    count = 1;
                }
                colNumber += count;
            }
            else {
                Position pos = SET_POS(colNumber, (BOARD_SIZE - 1) - rowNum);
                Square* sq = &state->board->squares[pos];
                int len = strlen(curToken);
                // j gets incremented in the loop
                for (int j = 0; j < len;) {
                    if (j < len && (curToken[j] == '1' || curToken[j] == '2')) {
                        Color color = (curToken[j] == '1') ? WHITE : BLACK;
                        j++;
                        Stone stone = FLAT;
                        if (j < len && (curToken[j] == 'S' || curToken[j] == 'C')) {
                            stone = (curToken[j] == 'S') ? STANDING : CAP;
                            j++;
                        }
                        Piece newPiece = (Piece) {stone, color};
                        sq->pieces[sq->numPieces++] = newPiece;
                    }
                    else {
                        printf("Invalid TPS format: token does not begin with a valid piece indicator\n");
                        printf("Token: %s\n", curToken);
                        freeGameState(state);
                        return NULL;
                    }
                }
                colNumber++;  
            }
            curToken = strtok_r(NULL, ",", &savePtr2);
        }
        // Check for extra tokens in this row.
        if (colNumber != BOARD_SIZE) {
            printf("Invalid number of columns in TPS on row %d (got %d, expected %d)\n", rowNum, colNumber, BOARD_SIZE);
            freeGameState(state);
            return NULL;
        }
    }
    updateReserves(state);
    updateBitboards(state);
    state->hash = computeBoardHash(state);
    double* vec = gameStateToVector(state);
    memcpy(state->gameVector, vec, sizeof(double) * 198);
    free(vec);
    /* printBoard(state->board); */
    return state;
}

// constructs tps backwards then flips the string
// this makes dealing with the linked lists of pieces easy
char* boardToTPS(Board* board) {
    char* boardStr = malloc(256);
    if (boardStr == NULL) {
        printf("Failed to allocate memory for TPS\n");
        return NULL;
    }
    u32 size = 0;

    // Iterate over rows 
    for (u8 row = 0; row < BOARD_SIZE; row++) {
        // if not first row add separator
        if (row != 0) {
            boardStr[size++] = '/';
        }

        u8 xs = 0;
        for (int col = BOARD_SIZE - 1; col >= 0; col--) {
            /* printf("Row: %d, Col: %d\n", row, col); */
            Square* sq = &board->squares[SET_POS(col, row)];

            if(sq->numPieces == 0) {
                xs++;
            }
            else {
                // handle previous empties
                if (xs > 0) {
                    if (!(size == 0) && !(boardStr[size - 1] == '/')) {
                        boardStr[size++] = ',';
                    }
                    if (xs > 1) {
                        boardStr[size++] = '0' + xs;
                    }
                    boardStr[size++] = 'x';
                    xs = 0;
                }

                if (!(size == 0) && !(boardStr[size - 1] == '/')) {
                    boardStr[size++] = ',';
                }

                // handle all pieces in a position
                for (int i = sq->numPieces - 1; i >= 0; i--) {
                    Piece cur = sq->pieces[i];
                    if (cur.stone == STANDING) {
                        boardStr[size++] = 'S';
                    }
                    else if (cur.stone == CAP) {
                        boardStr[size++] = 'C';
                    }
                    boardStr[size++] = (cur.color == WHITE) ? '1' : '2';
                }
            }
        }

        // handle left over xs
        if (xs > 0) {
            if (!(size == 0) && !(boardStr[size - 1] == '/')) {
                boardStr[size++] = ',';
            }
            if (xs > 1) {
                boardStr[size++] = '0' + xs;
            }
            boardStr[size++] = 'x';
        }
    }

    char* boardStrRev = malloc(size + 1);

    for (u8 i = 0; i < size; i++) {
        boardStrRev[i] = boardStr[size - 1 - i];
    }
    boardStrRev[size] = '\0';

    free(boardStr);
    return boardStrRev;
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
