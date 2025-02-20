#include "testGeneral.h"

int countMoves(const GameHistory* history) {
    int count = 0;
    const GameHistory* current = history;
    while (current != NULL) {
        count++;
        current = current->next;
    }
    return count;
}

Move* historyToMoveArray(const GameHistory* history, int* moveCount) {
    *moveCount = countMoves(history);

    if (*moveCount == 0) {
        return NULL;
    }

    Move* moves = (Move*)malloc(*moveCount * sizeof(Move));
    if (!moves) {
        perror("Memory allocation failed");
        *moveCount = 0;
        return NULL;
    }

    const GameHistory* current = history;
    for (int i = 0; i < *moveCount; i++) {
        moves[*moveCount - 1 - i] = *copyMove(&current->move);
        current = current->next;
    }

    return moves;
}

void validateMoves(GameState* board, Move* moves, int moveCount) {
    printf("Validating %d moves\n", moveCount);
    for (int i = 0; i < moveCount; i++) {

        printf("\n\n");
        printf("Validating move: ");
        printMove(&moves[i]);

        char* tps = boardToTPS(board->board);
        printf("Board state: %s\n", tps);

        GameState* oldBoard = copyGameState(board);
        MoveResult result = makeMoveChecks(board, &moves[i]);
        tps = boardToTPS(board->board);
        printf("Board state after move: %s\n", tps);

        if (result != SUCCESS) {
            fprintf(stderr, "Failed to make move: %d\n", result);
            exit(EXIT_FAILURE);
        } else {
            /* GameState* newBoard = makeMoveNoChecks(board, &moves[i]); */
            GameState* newBoard = copyGameState(board);
            /* printf("copied board\n"); */
            printf("attempting undo\n");
            MoveResult undoResult = undoMoveChecks(newBoard, &moves[i]);
            tps = boardToTPS(newBoard->board);
            printf("Board state after undoing move: %s\n", tps);
            free(tps);

            if (undoResult != SUCCESS) {
                fprintf(stderr, "Failed to undo move: %d\n", undoResult);
                freeGameState(oldBoard);
                exit(EXIT_FAILURE);
            } else {
                if (areEqualBoards(oldBoard, newBoard)) {
                    /* printf("Board state matches after undoing move\n"); */
                    freeGameState(oldBoard);
                    freeGameState(newBoard);
                } else {
                    fprintf(stderr, "Board state does not match after undoing move\n");
                    freeGameState(oldBoard);
                    freeGameState(newBoard);
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    freeGameState(board);
}

char** getPTNFiles(const char* dirPath, int* fileCount) {
    DIR* dir = opendir(dirPath);
    if (!dir) {
        perror("Failed to open directory");
        *fileCount = 0;
        return NULL;
    }

    struct dirent* entry;
    int count = 0;
    while ((entry = readdir(dir)) != NULL) {
        printf("Found file: %s\n", entry->d_name);
        if (entry->d_type == DT_REG) {
            const char* ext = strrchr(entry->d_name, '.');
            if (ext && strcmp(ext, ".ptn") == 0) {
                count++;
            }
        }
    }
    rewinddir(dir);

    char** filePaths = (char**)malloc(count * sizeof(char*));
    if (!filePaths) {
        perror("Memory allocation failed");
        closedir(dir);
        *fileCount = 0;
        return NULL;
    }

    int index = 0;
    while ((entry = readdir(dir)) != NULL && index < count) {
        if (entry->d_type == DT_REG) {
            const char* ext = strrchr(entry->d_name, '.');
            if (ext && strcmp(ext, ".ptn") == 0) {
                // +2 for '/' and null terminator
                size_t pathLen = strlen(dirPath) + strlen(entry->d_name) + 2;
                filePaths[index] = (char*)malloc(pathLen);
                if (filePaths[index]) {
                    snprintf(filePaths[index], pathLen, "%s/%s", dirPath, entry->d_name);
                    index++;
                }
            }
        }
    }

    closedir(dir);
    *fileCount = index;

    printf("Found PTN files:\n");
    for (int i = 0; i < index; i++) {
        printf("  %s\n", filePaths[i]);
    }

    return filePaths;
}

Move* readPTNFile(const char* filePath, int* moveCount) {
    FILE* file = fopen(filePath, "r");
    if (!file) {
        perror("Failed to open PTN file");
        *moveCount = 0;
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* contents = (char*)malloc(fileSize + 1);
    if (!contents) {
        perror("Memory allocation failed");
        fclose(file);
        *moveCount = 0;
        return NULL;
    }

    size_t bytesRead = fread(contents, 1, fileSize, file);
    fclose(file);

    if (bytesRead != (size_t)fileSize) {
        perror("Failed to read entire file");
        free(contents);
        *moveCount = 0;
        return NULL;
    }

    contents[fileSize] = '\0';

    PTN ptn;
    PTNParseError error = parsePTN(contents, &ptn);
    free(contents);

    if (error != PTN_PARSE_OK) {
        fprintf(stderr, "Failed to parse PTN file: %d\n", error);
        *moveCount = 0;
        return NULL;
    }

    Move* moves = historyToMoveArray(ptn.moves, moveCount);

    freePTN(&ptn);
    return moves;
}


// Implementation of areEqualBoards function
bool areEqualBoards(const GameState* b1, const GameState* b2) {
    char* tps1 = boardToTPS(b1->board);
    char* tps2 = boardToTPS(b2->board);
    printf("Board 1: %s\n", tps1);
    printf("Board 2: %s\n", tps2);

    bool result = (strcmp(tps1, tps2) == 0);

    free(tps1);
    free(tps2);
    return result;
}

void runGeneralTests(void) {
    int fileCount;
    char** files = getPTNFiles(".", &fileCount);

    if (!files || fileCount == 0) {
        fprintf(stderr, "No PTN files found\n");
        return;
    }

    for (int i = 0; i < fileCount; i++) {
        int moveCount;
        Move* moves = readPTNFile(files[i], &moveCount);

        if (moves && moveCount > 0) {
            printf("Validating moves from PTN file: %s\n", files[i]);

            GameState* emptyBoard = createGameState();

            validateMoves(emptyBoard, moves, moveCount);
            printf("Passed PTN file: %s\n", files[i]);

            free(moves);
        }

        free(files[i]);
    }

    free(files);
}


