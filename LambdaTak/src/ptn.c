#include "ptn.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

PTNParseError parsePTN(const char* input, PTN* ptn) {
    memset(ptn, 0, sizeof(PTN));

    char* lines[1024];
    int lineCount = 0;
    char* line = strtok((char*)input, "\n");
    while (line != NULL) {
        lines[lineCount++] = line;
        line = strtok(NULL, "\n");
    }

    for (int i = 0; i < lineCount; i++) {
        if (strncmp(lines[i], "[Site:", 6) == 0) {
            ptn->site = strdup(lines[i] + 6);
        } else if (strncmp(lines[i], "[Event:", 7) == 0) {
            ptn->event = strdup(lines[i] + 7);
        } else if (strncmp(lines[i], "[Date:", 6) == 0) {
            ptn->date = strdup(lines[i] + 6);
        } else if (strncmp(lines[i], "[Time:", 6) == 0) {
            ptn->time = strdup(lines[i] + 6);
        } else if (strncmp(lines[i], "[Player1:", 9) == 0) {
            ptn->player1 = strdup(lines[i] + 9);
        } else if (strncmp(lines[i], "[Player2:", 9) == 0) {
            ptn->player2 = strdup(lines[i] + 9);
        } else if (strncmp(lines[i], "[Clock:", 7) == 0) {
            ptn->clock = strdup(lines[i] + 7);
        } else if (strncmp(lines[i], "[Result:", 8) == 0) {
            ptn->result = strdup(lines[i] + 8);
        } else if (strncmp(lines[i], "[Size:", 6) == 0) {
            ptn->size = atoi(lines[i] + 6);
        }
    }

    ptn->moves = NULL;
    u8 flip = 0;
    for (int i = 0; i < lineCount; i++) {
        if (isdigit(lines[i][0])) {
            char* line_copy = strdup(lines[i]);
            if (!line_copy) return PTN_PARSE_ERROR;
            char* saveptr;
            strtok_r(line_copy, " ", &saveptr); // Skip move number
            Color current_color = WHITE;
            char* token;
            while ((token = strtok_r(NULL, " ", &saveptr)) != NULL) {
                Move move;
                PTNParseError err;
                if (flip < 2) {
                    err = parseMove(token, oppositeColor(current_color), &move);
                    flip++;
                }
                else {
                    err = parseMove(token, current_color, &move);
                }
                if (err != PTN_PARSE_OK) {
                    free(line_copy);
                    return err;
                }
                ptn->moves = addHistory(ptn->moves, move);
                current_color = (current_color == WHITE) ? BLACK : WHITE;
            }
            free(line_copy);
        }
    }

    return PTN_PARSE_OK;
}

PTNParseError parseMove(const char* moveStr, Color color, Move* move) {
    bool crush = (moveStr[strlen(moveStr) - 1] == '*');
    char* moveStrCopy = strdup(moveStr);
    if (crush) moveStrCopy[strlen(moveStrCopy) - 1] = '\0';

    if (strlen(moveStrCopy) == 2 && isalpha(moveStrCopy[0]) && isdigit(moveStrCopy[1])) {
        Position pos = {moveStrCopy[0] - 'a', moveStrCopy[1] - '1'};
        *move = createPlaceMove(pos, color, FLAT);
        free(moveStrCopy);
        return PTN_PARSE_OK;
    } else if (moveStrCopy[0] == 'S' && strlen(moveStrCopy) == 3 
            && isalpha(moveStrCopy[1]) && isdigit(moveStrCopy[2])) {
        Position pos = {moveStrCopy[1] - 'a', moveStrCopy[2] - '1'};
        *move = createPlaceMove(pos, color, STANDING);
        free(moveStrCopy);
        return PTN_PARSE_OK;
    } else if (moveStrCopy[0] == 'C' && strlen(moveStrCopy) == 3 
            && isalpha(moveStrCopy[1]) && isdigit(moveStrCopy[2])) {
        Position pos = {moveStrCopy[1] - 'a', moveStrCopy[2] - '1'};
        *move = createPlaceMove(pos, color, CAP);
        free(moveStrCopy);
        return PTN_PARSE_OK;
    } else {
        PTNParseError err = parseSlideMove(moveStrCopy, color, crush, move);
        free(moveStrCopy);
        return err;
    }
}

PTNParseError parseSlideMove(const char* str, Color color, bool crush, Move* move) {
    int count = 1;
    const char* ptr = str;

    if (isdigit(ptr[0])) {
        count = ptr[0] - '0';
        ptr++;
    }

    if (strlen(ptr) < 2) return PTN_POSITION_ERROR;
    Position pos = { ptr[0] - 'a', ptr[1] - '1' };
    ptr += 2;

    if (strlen(ptr) < 1) return PTN_DIRECTION_ERROR;
    Direction dir = charToDirection(ptr[0]);
    if (dir == -1) return PTN_DIRECTION_ERROR;
    ptr++;

    u8 drops[MAX_PICKUP];
    int dropCount = 0;

    while (*ptr != '\0' && isdigit(*ptr)) {
        printf("Drop: %c\n", *ptr);
        if (dropCount >= MAX_PICKUP) return PTN_COUNT_ERROR;
        drops[dropCount++] = *ptr - '0';
        ptr++;
    }

    if (dropCount == 0) {
        printf("No drops: count: %d\n", count);
        drops[0] = count;
        dropCount = 1;
    }

    for (;dropCount < MAX_PICKUP; dropCount++) {
        drops[dropCount] = 0;
    }

    // Create the slide move
    *move = createSlideMove(color, pos, dir, count, drops, crush ? CRUSH : NO_CRUSH);
    return PTN_PARSE_OK;
}

Direction charToDirection(char c) {
    switch (c) {
        case '+': return UP;
        case '-': return DOWN;
        case '>': return RIGHT;
        case '<': return LEFT;
        default: return -1;
    }
}

char directionToChar(Direction dir) {
    switch (dir) {
        case UP: return '+';
        case DOWN: return '-';
        case RIGHT: return '>';
        case LEFT: return '<';
        default: return '?';
    }
}

void freePTN(PTN* ptn) {
    if (ptn->site) free(ptn->site);
    if (ptn->event) free(ptn->event);
    if (ptn->date) free(ptn->date);
    if (ptn->time) free(ptn->time);
    if (ptn->player1) free(ptn->player1);
    if (ptn->player2) free(ptn->player2);
    if (ptn->clock) free(ptn->clock);
    if (ptn->result) free(ptn->result);
    if (ptn->moves) freeHistory(ptn->moves);
}
