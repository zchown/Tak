#include "moves.h"
#include "magic.h"

MoveResult checkMove(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        if (!isValidPosition(move->move.place.pos)) return INVALID_POSITION;
        if (readSquare(state->board, move->move.place.pos)->head != NULL) return INVALID_POSITION;
        if (move->move.place.color != state->turn) {
            if (state->turnNumber > 2) return INVALID_COLOR;
        }
        Stone stone = move->move.place.stone;
        Reserves* reserves = (move->move.place.color == WHITE) ? &state->player1 : &state->player2;
        if (reserves->stones == 0 && (stone == FLAT || stone == STANDING)) {
            return INVALID_STONE;
        }
        if (reserves->caps == 0 && stone == CAP) {
            return INVALID_STONE;
        }
        return SUCCESS;
    } 
    else if (move->type == SLIDE) {
        if (!isValidPosition(move->move.slide.startPos)) return INVALID_POSITION;
        Square* startSq = readSquare(state->board, move->move.slide.startPos);
        if (startSq->head == NULL) return INVALID_POSITION;
        if (startSq->head->color != move->move.slide.color) return INVALID_COLOR;
        if (move->move.slide.color != state->turn) return INVALID_COLOR;
        if (move->move.slide.count == 0 || move->move.slide.count > MAX_PICKUP) return INVALID_COUNT;
        if (move->move.slide.direction < 0 || move->move.slide.direction > 3) return INVALID_DIRECTION;

        // Check drops sum
        u8 sum = 0;
        u8 len = 0;
#pragma unroll
        for (u8 i = 0; i < MAX_DROPS; i++) {  // Max 5 groups in 16 bits
            u8 drop = (move->move.slide.drops >> (i * 3)) & 0x7; // Extract 3-bit group
            if (drop == 0) continue;
            sum += drop;
            if (sum > move->move.slide.count) {
                printf("Invalid sum: %d\n", sum);
                return INVALID_DROPS;
            }
            len++;
        }
        if (sum != move->move.slide.count) {
            printf("Invalid sum: %d\n", sum);
            return INVALID_DROPS;
        }

        // Check if the path is valid
        bool shouldCrush = false;
        Position nextPos = move->move.slide.startPos;
        for (u8 j = 0; j < len; j++) {
            nextPos = nextPosition(nextPos, move->move.slide.direction);
            if (!isValidPosition(nextPos)) return INVALID_SLIDE;
            Square* sq = readSquare(state->board, nextPos);
            if (sq->head != NULL) {
                if (j == len - 1) {  
                    if (sq->head->stone == CAP) return INVALID_SLIDE;
                    if (sq->head->stone == STANDING) {
                        if (startSq->head->stone != CAP) return INVALID_CRUSH;
                        if (move->move.slide.crush != CRUSH) return INVALID_CRUSH;
                        if (((move->move.slide.drops >> (j * 3)) & 0x7) != 1) return INVALID_CRUSH;
                        shouldCrush = true;
                    }
                } else {
                    if (sq->head->stone != FLAT) return INVALID_SLIDE;
                }
            }
        }
        if (shouldCrush && move->move.slide.crush != CRUSH) return INVALID_CRUSH;
        if (!shouldCrush && move->move.slide.crush == CRUSH) return INVALID_CRUSH;
        return SUCCESS;
    }
    return INVALID_MOVE_TYPE;
}

MoveResult makeMoveChecks(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        MoveResult result = checkMove(state, move);
        if (result != SUCCESS) {
            printf("Invalid move: %d\n", result);
            return result;
        }
        else {
            makeMoveNoChecks(state, move);
            return SUCCESS;
        }
    }
    else if (move->type == SLIDE) {
        MoveResult result = checkMove(state, move);
        if (result != SUCCESS) {
            return result;
        }
        else {
            makeMoveNoChecks(state, move);
            return SUCCESS;
        }
    }

    return INVALID_MOVE_TYPE;
}

GameState* makeMoveNoChecks(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        Square* sq = readSquare(state->board, move->move.place.pos);
        Piece* piece = createPiece(move->move.place.stone, move->move.place.color);
        squareInsertPiece(state, sq, piece);
        Reserves* reserves = (move->move.place.color == WHITE) ? &state->player1 : &state->player2;
        if (move->move.place.stone == FLAT || move->move.place.stone == STANDING) {
            reserves->stones--;
        } else {
            reserves->caps--;
        }
    } 
    else if (move->type == SLIDE) {
        Piece* pieces = squareRemovePieces(state, readSquare(state->board, move->move.slide.startPos), move->move.slide.count);

        u8 slideLength = 1;
        while (((move->move.slide.drops >> (slideLength * 3)) & 0x7) != 0) {
            slideLength++;
        }

        Position pos = slidePosition(move->move.slide.startPos, move->move.slide.direction, slideLength);
        if (move->move.slide.crush == CRUSH) {
            Square* sq = readSquare(state->board, pos);
            if (sq->head) {
                sq->head->stone = FLAT;
            }
        }

        Direction dir = oppositeDirection(move->move.slide.direction);
        for (u8 i = 0; i < slideLength; i++) {
            Square* sq = readSquare(state->board, pos);
            u8 dropCount = (move->move.slide.drops >> ((slideLength - i - 1) * 3)) & 0x7;
            pieces = squareInsertPieces(state, sq, pieces, dropCount);
            pos = nextPosition(pos, dir);
        }
    }

    state->turn = (state->turn == WHITE) ? BLACK : WHITE;
    state->turnNumber++;
    state->history = addHistory(state->history, *move);
    return state;
}

MoveResult undoMoveChecks(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        if (!isValidPosition(move->move.place.pos)) return INVALID_POSITION;
        if (readSquare(state->board, move->move.place.pos)->head == NULL) return INVALID_POSITION;
        if (readSquare(state->board, move->move.place.pos)->numPieces != 1) return INVALID_POSITION;
        undoMoveNoChecks(state, move);
        return SUCCESS;
    } 
    else if (move->type == SLIDE) {
        const SlideMove* mv = &move->move.slide;
        if (mv->color != oppositeColor(state->turn)) return INVALID_COLOR;
        if (!isValidPosition(mv->startPos)) return INVALID_POSITION;

        // Determine slide length from packed drops
        u8 slideLength = 0;
        u8 dropsTotal = 0;
        while (((mv->drops >> (slideLength * 3)) & 0x7) != 0 && slideLength < MAX_DROPS) {
            dropsTotal += (mv->drops >> (slideLength * 3)) & 0x7;
            slideLength++;
        }

        Position endPos = slidePosition(mv->startPos, mv->direction, slideLength);
        if (!isValidPosition(endPos)) return INVALID_POSITION;

        // Validate drops count
        if (dropsTotal != mv->count) return INVALID_COUNT;

        Position curPos = nextPosition(mv->startPos, mv->direction);
        u8 i = 0;
        dropsTotal = 0;

        while (i < slideLength) {
            u8 drop = (mv->drops >> (i * 3)) & 0x7;
            dropsTotal += drop;
            if (drop == 0) return INVALID_DROPS;
            if (drop > readSquare(state->board, curPos)->numPieces) return INVALID_DROPS;

            if ((positionToIndex(curPos) != positionToIndex(endPos)) &&
                    readSquare(state->board, curPos)->head->stone != FLAT) {
                return INVALID_SLIDE;
            } 
            else if (positionToIndex(curPos) == positionToIndex(endPos)) {
                if (mv->crush == CRUSH) {
                    if (readSquare(state->board, curPos)->numPieces < 2) return INVALID_CRUSH;
                    if (readSquare(state->board, curPos)->head->stone != CAP) return INVALID_CRUSH;
                    if (readSquare(state->board, curPos)->head->next->stone == CAP) return INVALID_CRUSH;
                }
            }
            i++;
            curPos = nextPosition(curPos, mv->direction);
        }

        undoMoveNoChecks(state, move);
    } 
    else {
        return INVALID_MOVE_TYPE;
    }
    return SUCCESS;
}

GameState* undoMoveNoChecks(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        Square* sq = readSquare(state->board, move->move.place.pos);
        squareRemovePiece(state, sq);
        Reserves* reserves = (move->move.place.color == WHITE) ? &state->player1 : &state->player2;
        if (move->move.place.stone == FLAT || move->move.place.stone == STANDING) {
            reserves->stones++;
        } else {
            reserves->caps++;
        }
    } 
    else if (move->type == SLIDE) {
        const SlideMove* mv = &move->move.slide;
        u8 slideLength = 1;
        while (((mv->drops >> (slideLength * 3)) & 0x7) != 0) {
            slideLength++;
        }

        Position endPos = slidePosition(mv->startPos, mv->direction, slideLength);
        Position curPos = endPos;
        Direction invDir = oppositeDirection(mv->direction);
        Piece* accHead = NULL;
        Piece* accTail = NULL;

        u8 curIndex = slideLength - 1;
        while (positionToIndex(curPos) != positionToIndex(mv->startPos)) {
            u8 dropCount = (mv->drops >> (curIndex * 3)) & 0x7;
            Piece* temp = squareRemovePieces(state, readSquare(state->board, curPos), dropCount);
            if (accHead) {
                accTail->next = temp;
                accTail = temp;
                while (accTail->next) {
                    accTail = accTail->next;
                }
            } else {
                accHead = temp;
                accTail = temp;
                while (accTail->next) {
                    accTail = accTail->next;
                }
            }
            curIndex--;
            curPos = nextPosition(curPos, invDir);
        }
        squareInsertPieces(state, readSquare(state->board, mv->startPos), accHead, mv->count);
    }

    state->turn = (state->turn == WHITE) ? BLACK : WHITE;
    state->turnNumber--;
    state->history = removeHead(state->history);
    return state;
}

GeneratedMoves* generateAllMoves(const GameState* state) {
    if (state->result != CONTINUE) return NULL;
    GeneratedMoves* toReturn = malloc(sizeof(GeneratedMoves));
    if (state->turnNumber <= 2) {
        Move* moves = malloc(36 * sizeof(Move));
        if (!moves) {
            printf("Memory allocation failed for moves array\n");
            return NULL;
        }

        u8 totalMoves = 0;
        for (u8 j = 0; j < TOTAL_SQUARES; j++) {
            if (state->emptySquares & (1ULL << j)) {
                moves[totalMoves++] = createPlaceMove(indexToPosition(j), oppositeColor(state->turn), FLAT);
            }
        }

        toReturn->moves = moves;
        toReturn->numMoves = totalMoves;
        return toReturn;
    }

    // https://theses.liacs.nl/pdf/LaurensBeljaards2017Tak.pdf
    Move* moves = malloc(256 * sizeof(Move));
    if (!moves) {
        printf("Memory allocation failed for moves array\n");
    }

    u32 totalMoves = 0;
    Reserves* res = (state->turn == WHITE) ? &state->player1 : &state->player2;
    Color turn = state->turn;

#pragma unroll
    for (u8 j = 0; j < TOTAL_SQUARES; j++) {
        if (state->emptySquares & (1ULL << j)) {
            Position pos = indexToPosition(j);
            if (res->stones > 0) {
                moves[totalMoves] = createPlaceMove(pos, turn, FLAT);
                totalMoves++;
                moves[totalMoves] = createPlaceMove(pos, turn, STANDING);
                totalMoves++;
            }
            if (res->caps > 0) {
                moves[totalMoves] = createPlaceMove(pos, turn, CAP);
                totalMoves++;
            }
        }
    }

#pragma unroll
    for (u8 j = 0; j < TOTAL_SQUARES; j++) {
        Bitboard* control = (state->turn == WHITE) ? &state->whiteControlled : &state->blackControlled;
        if (*control & (1ULL << j)) {
            Position pos = indexToPosition(j);
            generateSlidesInDir(state, pos, LEFT, moves, &totalMoves);
            generateSlidesInDir(state, pos, RIGHT, moves, &totalMoves);
            generateSlidesInDir(state, pos, UP, moves, &totalMoves);
            generateSlidesInDir(state, pos, DOWN, moves, &totalMoves);
        }
    }

    toReturn->moves = moves;
    toReturn->numMoves = totalMoves;
    return toReturn;
}

void generateSlidesInDir(const GameState* state, Position pos, Direction dir, Move* moves, u32* totalMoves) {
    Square* sq = readSquare(state->board, pos);
    u8 maxCount = (sq->numPieces < MAX_PICKUP) ? sq->numPieces : MAX_PICKUP;
    Color turn = state->turn;
    Board* board = state->board;

    u8 steps = numSteps(state, pos, dir);
    /* printf("Position: %d %d\n", pos.x, pos.y); */
    /* printf("Direction: %d\n", dir); */
    /* printf("Steps: %d\n", steps); */
    /* printf("Max count: %d\n", maxCount); */
    if (steps != 0) {
        for (u8 curCount = 1; curCount <= maxCount; curCount++) {
            u32 numberOfSlides = countValidSequences(curCount, steps);
            /* printf("drop seq: %d, %d\n", curCount, steps); */
            u16* sequences = dropSequence(curCount, steps);
            for (u8 i = 0; i < numberOfSlides; i++) {
                moves[*totalMoves] = createSlideMove(turn, pos, dir, curCount, sequences[i], NO_CRUSH);
                (*totalMoves)++;
            }
            /* free(sequences); */
        }
    }

    // Determine if we have a crush situation
    Position crushPos = slidePosition(pos, dir, steps + 1);
    Crush canCrush = 
        (readSquare(board, pos)->head->stone == CAP) && 
        (isValidPosition(crushPos)) && (steps + 1 <= maxCount) &&
        (readSquare(board, crushPos)->head != NULL) && 
        (readSquare(board, crushPos)->head->stone == STANDING) 
        ? CRUSH : NO_CRUSH;

    if (canCrush == CRUSH) {
        if (steps == 0 || maxCount == 1) {
            moves[*totalMoves] = createSlideMove(turn, pos, dir, 1, 1, CRUSH);
            (*totalMoves)++;
            return;
        }

        for (u8 curCount = steps + 1; curCount <= maxCount; curCount++) {
            u32 numberOfSlides = binomialCoefficient(curCount - 1, steps - 1);
            u16* sequences = dropSequencesForCrush(curCount, steps);
            for (u8 i = 0; i < numberOfSlides; i++) {
                moves[*totalMoves] = createSlideMove(turn, pos, dir, curCount, sequences[i], CRUSH);
                (*totalMoves)++;
            }
            /* free(sequences); */
        }
    }
}

/* 
   The following explains what we will do for dropSequence and its
   associates helper functions.

https://artofproblemsolving.com/wiki/index.php/Ball-and-urn#Restrictions 

Even though our actual problem is using up to (count) balls 
here we are going to use all (count) balls and iteration through
all possible counts when we are generating moves.

This is a specific case where we want the balls to be left aligned.
If a space is empty all spaces to the right of it must also be empty.

To do this we can look at the restriction of this problem where we have
k = 1 each urn must have at least one ball. If we have up to (spaces) to work
with we can have m spaces and sum for all valid ms 1 <= m <= spaces.
*/
#pragma inline
u16* dropSequence(u8 count, u8 spaces) {
    return dseq[count * BOARD_SIZE + spaces - 1];
    /* u32 total = countValidSequences(count, spaces); */
    /* uint16_t* sequences = malloc(total * sizeof(uint16_t)); */
    /*  */
    /* if (!sequences) { */
    /*     printf("Memory allocation failed for sequences array\n"); */
    /*     return NULL; */
    /* } */
    /*  */
    /* u8 index = 0; */
    /* for (u8 spacesFilled = 1; spacesFilled <= spaces; spacesFilled++) { */
    /*     u32 numConfigurations = binomialCoefficient(count - 1, spacesFilled - 1); */
    /*     if (numConfigurations == 0) continue; */
    /*  */
    /*     for (u8 configIdx = 0; configIdx < numConfigurations; configIdx++) { */
    /*         uint16_t packedDrops = 0; */
    /*         u8 remaining = count; */
    /*         u8 usedSpaces = 0; */
    /*  */
    /*         for (u8 i = 0; i < spacesFilled - 1; i++) { */
    /*             u8 dropValue = (remaining > 1) ? (remaining - (spacesFilled - i - 1)) : 1; */
    /*             packedDrops |= (dropValue << (i * 3)); */
    /*             remaining -= dropValue; */
    /*             usedSpaces++; */
    /*         } */
    /*  */
    /*         packedDrops |= (remaining << (usedSpaces * 3));  // Place last value in remaining space */
    /*         sequences[index + configIdx] = packedDrops; */
    /*     } */
    /*  */
    /*     index += numConfigurations; */
    /* } */
    /*  */
    /* u16* alt = dseq[count * BOARD_SIZE + spaces - 1]; */
    /* for (u8 i = 0; i < total; i++) { */
    /*     if (sequences[i] != alt[i]) { */
    /*         printf("Count: %d, Spaces: %d\n", count, spaces); */
    /*         printf("Mismatch at index %d: %d %d\n", i, sequences[i], alt[i]); */
    /*     } */
    /* } */
    /*  */
    /* return sequences; */
}

/*
   This is an easier function as we just have to generate all possible
   combinations of putting (count) balls into (spaces) where we have to
   use all spaces. And for the crush the last space must be a 1.
   */
#pragma inline
u16* dropSequencesForCrush(u8 count, u8 spaces) {
    return dseqcrush[count * BOARD_SIZE + spaces - 1];
    /* if (count < spaces) { */
    /*     printf("Invalid input: count must be greater than or equal to spaces\n"); */
    /*     return NULL; */
    /* } */
    /*  */
    /* u32 total = binomialCoefficient(count - 1, spaces - 1); */
    /* uint16_t* sequences = malloc(total * sizeof(uint16_t)); */
    /*  */
    /* if (!sequences) { */
    /*     printf("Memory allocation failed for sequences array\n"); */
    /*     return NULL; */
    /* } */
    /*  */
    /* for (u8 i = 0; i < total; i++) { */
    /*     uint16_t packedDrops = 0; */
    /*     u8 remaining = count - 1; */
    /*     u8 usedSpaces = 0; */
    /*  */
    /*     for (u8 j = 0; j < spaces - 1; j++) { */
    /*         u8 dropValue = (remaining > 1) ? (remaining - (spaces - j - 1)) : 1; */
    /*         packedDrops |= (dropValue << (j * 3)); */
    /*         remaining -= dropValue; */
    /*         usedSpaces++; */
    /*     } */
    /*  */
    /*     packedDrops |= (1 << (usedSpaces * 3));  // Ensure the last value is always 1 for crush */
    /*     sequences[i] = packedDrops; */
    /* } */
    /*  */
    /* return sequences; */
}

#pragma inline
u8 binomialCoefficient(u8 n, u8 k) {
    return binCoe[n * MAX_DROPS + k];
    /* if (k > n) return 0; */
    /* if (k == 0 || k == n) return 1; */
    /*  */
    /* if (k > n - k) k = n - k; */
    /*  */
    /* u8 result = 1; */
    /* for (u8 i = 0; i < k; i++) { */
    /*     result = result * (n - i) / (i + 1); */
    /* } */
    /*  */
    /* return result; */
}

#pragma inline
u8 countValidSequences(u8 count, u8 spaces) {
    return countValSeq[count * BOARD_SIZE + spaces];
    /* u8 total = 0; */
    /* if (count == 0 || spaces == 0) return 0; */
    /* for (u8 m = 1; m <= spaces; m++) { */
    /*     total += binomialCoefficient(count - 1, m - 1); */
    /* } */
    /* return total; */
}


u8 numSteps(const GameState* state, Position pos, Direction dir) {
    u8 steps = 0;
    Position nextPos = nextPosition(pos, dir);
    while (isValidPosition(nextPos)) {
        Square* sq = readSquare(state->board, nextPos);
        if (sq->head && sq->head->stone != FLAT) {
            break;
        }
        else {
            steps++;
            nextPos = nextPosition(nextPos, dir);
        }
    }
    return steps;
}

void freeGeneratedMoves(GeneratedMoves* moves) {
    if (!moves) return;
    /* for (u32 i = 0; i < moves->numMoves; i++) { */
    /*     freeMove(&moves->moves[i]); */
    /* } */
    free(moves->moves);
    free(moves);
}
