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
    for (int i = 0; i < BOARD_SIZE; i++) {
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
                for (int j = 0; j < strlen(curToken); j++) {
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


