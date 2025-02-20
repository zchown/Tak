#include "moves.h"

MoveResult checkMove(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        if (!isValidPosition(move->move.place.pos)) return INVALID_POSITION;
        if (readSquare(state->board, move->move.place.pos)->head != NULL) return INVALID_POSITION;
        if (move->move.place.color != state->turn) {
            if (state->turnNumber > 2) {
                return INVALID_COLOR;
            }
        }
        Stone stone = move->move.place.stone;
        if (state->player1.stones == 0 && (stone == FLAT || stone == STANDING)) {
            return INVALID_STONE;
        }
        if (state->player1.caps == 0 && stone == CAP) {
            return INVALID_STONE;
        }
        return SUCCESS;
    }
    else if (move->type == SLIDE) {
        if (!isValidPosition(move->move.slide.startPos)) return INVALID_POSITION;
        Square* startSq = readSquare(state->board, move->move.slide.startPos);
        if (startSq->head == NULL) return INVALID_POSITION;
        if (startSq->head->color != move->move.slide.color) {
            printf("StartSq->head->color: %d\n", startSq->head->color);
            printf("Move->move.slide.color: %d\n", move->move.slide.color);
            printMove(move);
            return INVALID_COLOR;
        }
        if (move->move.slide.color != state->turn) return INVALID_COLOR;
        if (move->move.slide.count == 0) return INVALID_COUNT;
        if (move->move.slide.count > MAX_PICKUP) return INVALID_COUNT;
        // enums are just ints so we can check if the value is in the range
        if (move->move.slide.direction < 0 || move->move.slide.direction > 3) return INVALID_DIRECTION;

        //check drops
        u8 sum = 0;
        u8 i = 0;
        u8 len = 0;
        while (move->move.slide.drops[i] != 0) {
            sum += move->move.slide.drops[i];
            if (sum > move->move.slide.count) {
                printf("Sum: %d, count: %d\n", sum, move->move.slide.count);
                return INVALID_DROPS;
            }
            i++;
            len++;
        }
        if (sum != move->move.slide.count) {
            printf("Sum: %d, count: %d\n", sum, move->move.slide.count);
            return INVALID_DROPS;
        }

        // check if anything in way and if crush is valid
        bool shouldCrush = false;
        for (u8 j = 0; j < len; j++) {
            Position nextPos = nextPosition(move->move.slide.startPos, move->move.slide.direction);
            /* printf("NextPos: %d %d\n", nextPos.x, nextPos.y); */
            if (!isValidPosition(nextPos)) return INVALID_SLIDE;
            Square* sq = readSquare(state->board, nextPos);
            if (sq->head != NULL) {
                if (j == len - 1) {
                    if (sq->head->stone == CAP) return INVALID_SLIDE; 
                    if (sq->head->stone == STANDING) {
                        /* printf("Sq->head->stone: %d\n", sq->head->stone); */
                        /* printf("Pos: %d %d\n", nextPos.x, nextPos.y); */
                        if (startSq->head->stone != CAP) return INVALID_CRUSH;
                        if (move->move.slide.crush != CRUSH) return INVALID_CRUSH;
                        if (move->move.slide.drops[j] != 1) return INVALID_CRUSH;
                        shouldCrush = true;
                        /* printf("Crush\n"); */
                    }
                }
                else {
                    if (sq->head->stone != FLAT) return INVALID_SLIDE;
                }
            }
        }
        if (shouldCrush && move->move.slide.crush != CRUSH) return INVALID_CRUSH;
        if (!shouldCrush && move->move.slide.crush == CRUSH) return INVALID_CRUSH;
        return SUCCESS;
    }
    else {
        return INVALID_MOVE_TYPE;
    }
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
        }
        else {
            reserves->caps--;
        }
    }
    else if (move->type == SLIDE) {
        Piece* pieces = squareRemovePieces(state, readSquare(state->board, move->move.slide.startPos), move->move.slide.count);
        u8 slideLength = 0;
        while (move->move.slide.drops[slideLength] != 0) {
            slideLength++;
        }
        Position pos = slidePosition(move->move.slide.startPos, move->move.slide.direction, slideLength);
        if (move->move.slide.crush == CRUSH) {
            Square* sq = readSquare(state->board, pos);
            if (!sq->head) {
                return state;
            }
            else {
                sq->head->stone = FLAT;
            }
        }
        Direction dir = oppositeDirection(move->move.slide.direction);
        for (u8 i = 0; i < slideLength; i++) {
            Square* sq = readSquare(state->board, pos);
            pieces = squareInsertPieces(state, sq, pieces, move->move.slide.drops[i]);
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
        u8 slideLength = 0;
        while (mv->drops[slideLength] != 0 && slideLength != MAX_PICKUP) {
            slideLength++;
        }
        Position endPos = slidePosition(mv->startPos, mv->direction, slideLength);
        if (!isValidPosition(endPos)) return INVALID_POSITION;

        Position curPos = nextPosition(mv->startPos, mv->direction);
        u8 i = 0;
        u8 dropsTotal = 0;
        while(i < slideLength) {
            dropsTotal += mv->drops[i];
            if (mv->drops[i] == 0) return INVALID_DROPS;
            if (mv->drops[i] > readSquare(state->board, curPos)->numPieces) return INVALID_DROPS;

            if((positionToIndex(curPos) != positionToIndex(endPos)) 
                    && readSquare(state->board, curPos)->head->stone != FLAT) {
                return INVALID_SLIDE;
            }
            else if(positionToIndex(curPos) == positionToIndex(endPos)) {
                if (mv->crush == CRUSH) {
                    if (readSquare(state->board, curPos)->numPieces < 2) return INVALID_CRUSH;
                    if (readSquare(state->board, curPos)->head->stone != CAP) return INVALID_CRUSH;
                    if (readSquare(state->board, curPos)->head->next->stone == CAP) return INVALID_CRUSH;
                }
            }
            i++;
            curPos = nextPosition(curPos, mv->direction);
        }

        if (dropsTotal != mv->count) {
            printf("Drops total: %d, count: %d\n", dropsTotal, mv->count);
            return INVALID_COUNT;
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
        }
        else {
            reserves->caps++;
        }
    }
    else if (move->type == SLIDE) {
        const SlideMove* mv = &move->move.slide;

        u8 slideLength = 0;
        while (mv->drops[slideLength] != 0 && slideLength < MAX_PICKUP) {
            slideLength++;
        }

        Position endPos = slidePosition(mv->startPos, mv->direction, slideLength);
        Position curPos = endPos;
        Direction invDir = oppositeDirection(mv->direction);
        Piece* accHead = NULL;
        Piece* accTail = NULL;

        u8 curIndex = slideLength - 1;

        while (positionToIndex(curPos) != positionToIndex(mv->startPos)) {
            Piece * temp = squareRemovePieces(state, readSquare(state->board, curPos), mv->drops[curIndex]);
            if (accHead) {
                accTail->next = temp;
                accTail = temp;
                while (accTail->next) {
                    accTail = accTail->next;
                }
            }
            else {
                accHead = temp;
                accTail = temp;
                while (accTail->next) {
                    accTail = accTail->next;
                }
            }
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
        /* int* es = emptySquares(state); */
        /* if (!es) { */
        /*     free(moves); */
        /*     return NULL; */
        /* } */
        /* u8 i = 0; */
        /* while (es[i] != -1 && i < n) { */
        /*     moves[i] = createPlaceMove(indexToPosition(es[i]), state->turn, FLAT); */
        /*     i++; */
        /* } */
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

    // overestimate the number of possible moves (I hope)
    // a real board will almost certainly have less than 2048 possible moves
    // https://theses.liacs.nl/pdf/LaurensBeljaards2017Tak.pdfhttps://theses.liacs.nl/pdf/LaurensBeljaards2017Tak.pdf
    Move* moves = malloc(512 * sizeof(Move));
    if (!moves) {
        printf("Memory allocation failed for moves array\n");
        return NULL;
    }
    u32 totalMoves = 0;

    // generate place moves
    /* int* es = emptySquares(state); */
    /* u8 i = 0; */
    /* while (es[i] != -1) { */
    /*     if (state->player1.stones > 0) { */
    /*         moves[totalMoves] = createPlaceMove(indexToPosition(es[i]), state->turn, FLAT); */
    /*         totalMoves++; */
    /*  */
    /*         moves[totalMoves] = createPlaceMove(indexToPosition(es[i]), state->turn, STANDING); */
    /*         totalMoves++; */
    /*     } */
    /*     if (state->player1.caps > 0) { */
    /*         moves[totalMoves] = createPlaceMove(indexToPosition(es[i]), state->turn, CAP); */
    /*         totalMoves++; */
    /*     } */
    /*     i++; */
    /* } */
    for (u8 j = 0; j < TOTAL_SQUARES; j++) {
        if (state->emptySquares & (1ULL << j)) {
            if (state->player1.stones > 0) {
                moves[totalMoves] = createPlaceMove(indexToPosition(j), state->turn, FLAT);
                totalMoves++;

                moves[totalMoves] = createPlaceMove(indexToPosition(j), state->turn, STANDING);
                totalMoves++;
            }
            if (state->player1.caps > 0) {
                moves[totalMoves] = createPlaceMove(indexToPosition(j), state->turn, CAP);
                totalMoves++;
            }
        }
    }

    // generate slide moves
    /* int* cs = controlledSquares(state, state->turn); */
    /* i = 0; */
    /* while (cs[i] != -1) { */
    /*     generateSlidesInDir(state, indexToPosition(cs[i]), LEFT, moves, &totalMoves); */
    /*     generateSlidesInDir(state, indexToPosition(cs[i]), RIGHT, moves, &totalMoves); */
    /*     generateSlidesInDir(state, indexToPosition(cs[i]), UP, moves, &totalMoves); */
    /*     generateSlidesInDir(state, indexToPosition(cs[i]), DOWN, moves, &totalMoves); */
    /*     i++; */
    /* } */
    for (u8 j = 0; j < TOTAL_SQUARES; j++) {
        Bitboard* control = (state->turn == WHITE) ? &state->whiteControlled : &state->blackControlled;
        if (*control & (1ULL << j)) {
            generateSlidesInDir(state, indexToPosition(j), LEFT, moves, &totalMoves);
            generateSlidesInDir(state, indexToPosition(j), RIGHT, moves, &totalMoves);
            generateSlidesInDir(state, indexToPosition(j), UP, moves, &totalMoves);
            generateSlidesInDir(state, indexToPosition(j), DOWN, moves, &totalMoves);
        }
    }

    toReturn->moves = moves;
    toReturn->numMoves = totalMoves;
    return toReturn;
}

void generateSlidesInDir(const GameState* state, Position pos, Direction dir, Move* moves, u32* totalMoves) {
    u8 steps = numSteps(state, pos, dir);
    // generate all possible slides no crush
    Square* sq = readSquare(state->board, pos);
    u8 maxCount = (sq->numPieces < MAX_PICKUP) ? sq->numPieces : MAX_PICKUP;

    for (u8 curCount = 1; curCount <= maxCount; curCount++) {
        u32 numberOfSlides = countValidSequences(curCount, steps);
        u8** sequences = dropSequence(curCount, steps);
        for (u8 i = 0; i < numberOfSlides; i++) {
            moves[*totalMoves] = 
                createSlideMove(state->turn, pos, dir, curCount, sequences[i], NO_CRUSH);
            (*totalMoves)++;
            free(sequences[i]);
        }
        free(sequences);
    }

    // determine if we have a crush situation
    Position crushPos = slidePosition(pos, dir, steps + 1);
    Crush canCrush = 
        (readSquare(state->board, pos)->head->stone == CAP) && 
        (isValidPosition(crushPos)) && 
        (readSquare(state->board, crushPos)->head != NULL) && 
        (readSquare(state->board, crushPos)->head->stone == STANDING) 
        ? CRUSH : NO_CRUSH;

    if (canCrush == CRUSH) {

        //special case
        if (steps == 0) {
            moves[*totalMoves] = 
                createSlideMove(state->turn, pos, dir, 1, (u8[]){1}, CRUSH);
            (*totalMoves)++;
            return;
        }

        // generate all possible slides with crush
        for (u8 curCount = steps+1; curCount <= maxCount; curCount++) {
            u32 numberOfSlides = binomialCoefficient(curCount - 1, steps - 1);
            u8** sequences = dropSequencesForCrush(curCount, steps);
            for (u8 i = 0; i < numberOfSlides; i++) {
                moves[*totalMoves] = 
                    createSlideMove(state->turn, pos, dir, curCount, sequences[i], CRUSH);
                (*totalMoves)++;
                free(sequences[i]);
            }
            free(sequences);
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
u8** dropSequence(u8 count, u8 spaces) {
    // Calculate how many valid configurations exist
    u32 total = countValidSequences(count, spaces);

    // Allocate memory for pointers to each configuration
    u8** sequences = malloc(total * sizeof(u8*));
    if (!sequences) {
        printf("Memory allocation failed for sequences array\n");
        return NULL;
    }

    // Allocate memory for each individual configuration
    for (u8 i = 0; i < total; i++) {
        sequences[i] = malloc(spaces * sizeof(u8));
        if (!sequences[i]) {
            printf("Memory allocation failed for sequence %u\n", i);
            // Clean up already allocated memory
            for (u8 j = 0; j < i; j++) {
                free(sequences[j]);
            }
            free(sequences);
            return NULL;
        }
    }

    // Generate all valid configurations
    u8 index = 0;
    for (u8 spacesFilled = 1; spacesFilled <= spaces; spacesFilled++) {
        // Calculate how many ways we can choose which spaces to fill
        u32 numConfigurations = binomialCoefficient(count - 1, spacesFilled - 1);
        if (numConfigurations == 0) continue;

        // Generate each possible configuration for this number of filled spaces
        for (u8 configIdx = 0; configIdx < numConfigurations; configIdx++) {
            u8* currentConfig = sequences[index + configIdx];

            // Special case: all balls in first container
            if (spacesFilled == 1) {
                currentConfig[0] = count;
            } 
            else {
                u32 remaining = configIdx;
                u32 n = count - 1;

                for (u8 i = 0; i < spacesFilled - 1; i++) {
                    u32 current = (i == 0) ? 0 : currentConfig[i - 1] + 1;

                    while (1) {
                        u32 combinations = binomialCoefficient(n - current - 1, spacesFilled - i - 2);
                        if (remaining < combinations) break;
                        remaining -= combinations;
                        current++;
                    }

                    currentConfig[i] = current;
                    n = n - current - 1;
                }

                u8 lastIndex = currentConfig[spacesFilled - 2];

                currentConfig[spacesFilled - 1] = (count - 1) - lastIndex;

                for (u8 i = spacesFilled - 2; i > 0; i--) {
                    currentConfig[i] = currentConfig[i] - currentConfig[i - 1];
                }

                // Handle the first position separately
                currentConfig[0] = currentConfig[0] + 1;
            }

            // Fill remaining containers with zeros
            for (u8 i = spacesFilled; i < spaces; i++) {
                currentConfig[i] = 0;
            }
        }

        index += numConfigurations;
    }

    return sequences;
}

/*
   This is an easier function as we just have to generate all possible
   combinations of putting (count) balls into (spaces) where we have to
   use all spaces. And for the crush the last space must be a 1.
   */
u8** dropSequencesForCrush(u8 count, u8 spaces) {
    // this should be checked when calling this function
    // but we will check it here as well
    if (count < spaces) {
        printf("Invalid input: count must be greater than or equal to spaces\n");
        return NULL;
    }

    u32 total = binomialCoefficient(count - 1, spaces - 1);

    u8** sequences = malloc(total * sizeof(u8*));
    if (!sequences) {
        printf("Memory allocation failed for sequences array\n");
        return NULL;
    }

    // +1 for the crush
    for (u8 i = 0; i < total; i++) {
        sequences[i] = malloc((spaces + 1) * sizeof(u8));
        if (!sequences[i]) {
            printf("Memory allocation failed for sequence %u\n", i);
            for (u8 j = 0; j < i; j++) {
                free(sequences[j]);
            }
            free(sequences);
            return NULL;
        }
    }

    u8 index = 0;
    u8 remainingPieces = count - spaces;

    for (u8 i = 0; i < total; i++) {
        for (u8 j = 0; j < spaces; j++) {
            sequences[i][j] = 1;
        }
        sequences[i][spaces] = 1;
    }

    for (u8 i = 0; i < total; i++) {
        if (i > 0) { // Skip the first one, which is already set to 1 for each space
            u32 remaining = i;
            u32 pieces = remainingPieces;
            u8 pos = spaces - 1;

            while (pieces > 0 && pos > 0) {
                u32 maxCombinations = binomialCoefficient(pieces + pos - 1, pos - 1);
                u8 usePieces = 0;

                while (remaining >= maxCombinations) {
                    remaining -= maxCombinations;
                    usePieces++;
                    if (pieces > usePieces) {
                        maxCombinations = maxCombinations * (pieces - usePieces) / (pieces + pos - usePieces);
                    }
                    else {
                        break;
                    }
                }

                sequences[i][pos - 1] += usePieces;
                pieces -= usePieces;
                pos--;
            }

            if (pieces > 0) {
                sequences[i][0] += pieces;
            }
        }
    }

    return sequences;
}

u8 binomialCoefficient(u8 n, u8 k) {
    if (k > n) return 0;
    if (k == 0 || k == n) return 1;

    if (k > n - k) k = n - k;

    u8 result = 1;
    for (u8 i = 0; i < k; i++) {
        result = result * (n - i) / (i + 1);
    }

    return result;
}

u8 countValidSequences(u8 count, u8 spaces) {
    u8 total = 0;
    if (count == 0 || spaces == 0) return 0;
    for (u8 m = 1; m <= spaces; m++) {
        total += binomialCoefficient(count - 1, m - 1);
    }
    return total;
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
    free(moves);
}
