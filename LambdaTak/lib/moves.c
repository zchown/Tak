#include "moves.h"
#include "board.h"
#include "magic.h"

MoveResult checkMove(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        if (!VALID_POSITION(move->move.place.pos)) return INVALID_POSITION;
        if (readSquare(state->board, move->move.place.pos)->numPieces != 0) return INVALID_POSITION;
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
        if (!VALID_POSITION(move->move.slide.startPos)) return INVALID_POSITION;
        Square* startSq = readSquare(state->board, move->move.slide.startPos);
        if (startSq->numPieces < move->move.slide.count) return INVALID_POSITION;
        if (SQ_HEAD(startSq).color != move->move.slide.color) return INVALID_COLOR;
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
                return INVALID_DROPS;
            }
            len++;
        }
        if (sum != move->move.slide.count) {
            return INVALID_DROPS;
        }

        // Check if the path is valid
        bool shouldCrush = false;
        Position nextPos = move->move.slide.startPos;
        for (u8 j = 0; j < len; j++) {
            nextPos = nextPosition(nextPos, move->move.slide.direction);
            if (!VALID_POSITION(nextPos)) return INVALID_SLIDE;
            Square* sq = readSquare(state->board, nextPos);
            if (sq->numPieces != 0) {
                if (j == len - 1) {  
                    if (SQ_HEAD(sq).stone == CAP) return INVALID_SLIDE;
                    if (SQ_HEAD(sq).stone == STANDING) {
                        if (SQ_HEAD(startSq).stone != CAP) {
                            /* printf("Crush with non capstone\n"); */
                            return INVALID_CRUSH;
                        }
                        if (move->move.slide.crush != CRUSH) {
                            /* printf("Crush with no crush\n"); */
                            return INVALID_CRUSH;
                        }
                        if (((move->move.slide.drops >> (j * 3)) & 0x7) != 1) {
                            /* printf("Crush with more than 1 drop\n"); */
                            return INVALID_CRUSH;
                        }
                        shouldCrush = true;
                    }
                } else {
                    if (SQ_HEAD(sq).stone != FLAT) return INVALID_SLIDE;
                }
            }
        }
        if (shouldCrush && move->move.slide.crush != CRUSH) {
            /* printf("Should crush but no crush\n"); */
            return INVALID_CRUSH;
        }
        if (!shouldCrush && move->move.slide.crush == CRUSH) {
            /* printf("Should not crush but crush\n"); */
            return INVALID_CRUSH;
        }
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
            makeMoveNoChecks(state, move, true);
            return SUCCESS;
        }
    }
    else if (move->type == SLIDE) {
        MoveResult result = checkMove(state, move);
        if (result != SUCCESS) {
            return result;
        }
        else {
            makeMoveNoChecks(state, move, true);
            return SUCCESS;
        }
    }

    return INVALID_MOVE_TYPE;
}

GameState* makeMoveNoChecks(GameState* state, const Move* move, bool doHistory) {
    if (move->type == PLACE) {
        const PlaceMove* mv = &move->move.place;
        Square* sq = readSquare(state->board, mv->pos);
        Piece piece = (Piece){mv->stone, mv->color};
        squareInsertPiece(state, sq, piece);
        /* updateSquareVector(state, mv->pos); */
        Reserves* reserves = (mv->color == WHITE) ? &state->player1 : &state->player2;
        if (mv->stone == FLAT || mv->stone == STANDING) {
            reserves->stones--;
        } else {
            reserves->caps--;
        }
    } 
    else if (move->type == SLIDE) {
        state->hash = clearSlideHash(state->hash, &move->move.slide, state);

        const SlideMove* mv = &move->move.slide;
        Square* startSq = readSquare(state->board, mv->startPos);
        PieceStack stack = squareRemovePieces(state, startSq, mv->count);
        /* updateSquareVector(state, mv->startPos); */

        u8 slideLength = 0;
        while (slideLength < MAX_DROPS && ((mv->drops >> (slideLength * 3)) & 0x7)) {
            slideLength++;
        }

        Position pos = slidePosition(mv->startPos, mv->direction, slideLength);
        if (mv->crush == CRUSH) {
            Square* sq = readSquare(state->board, pos);
            if (sq->numPieces > 0) {
                sq->pieces[sq->numPieces - 1].stone = FLAT;
                state->standingStones &= ~positionToBit(pos);
            }
        }

        Direction dir = oppositeDirection(mv->direction);
        for (u8 i = 0; i < slideLength; i++) {
            u8 dropCount = (mv->drops >> ((slideLength - i - 1) * 3)) & 0x7;
            if (dropCount == 0 || stack.numPieces < dropCount) break;

            PieceStack tempStack;
            tempStack.numPieces = dropCount;
            memcpy(tempStack.pieces, &stack.pieces[stack.numPieces - dropCount], dropCount * sizeof(Piece));
            stack.numPieces -= dropCount;

            Square* targetSq = readSquare(state->board, pos);
            squareInsertPieces(state, targetSq, &tempStack);
            pos = nextPosition(pos, dir);
        }
    }

    state->turn = oppositeColor(state->turn);
    state->turnNumber++;
    state->hash = incrementalUpdateHash(state->hash, move, state);
    /* state->hash = computeBoardHash(state); */
    if (doHistory) {
        state->history = addHistory(state->history, *move);
    }
    /* updateBitboards(state); */
    return state;
}

MoveResult undoMoveChecks(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        if (!VALID_POSITION(move->move.place.pos)) return INVALID_POSITION;
        if (readSquare(state->board, move->move.place.pos)->numPieces != 1) return INVALID_POSITION;
        undoMoveNoChecks(state, move, true);
        return SUCCESS;
    } 
    else if (move->type == SLIDE) {
        const SlideMove* mv = &move->move.slide;
        if (mv->color != oppositeColor(state->turn)) return INVALID_COLOR;
        if (!VALID_POSITION(mv->startPos)) return INVALID_POSITION;

        // Determine slide length from packed drops
        u8 slideLength = 0;
        u8 dropsTotal = 0;
        while (((mv->drops >> (slideLength * 3)) & 0x7) != 0 && slideLength < MAX_DROPS) {
            dropsTotal += (mv->drops >> (slideLength * 3)) & 0x7;
            slideLength++;
        }

        Position endPos = slidePosition(mv->startPos, mv->direction, slideLength);
        if (!VALID_POSITION(endPos)) return INVALID_POSITION;

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

            if (curPos != endPos && SQ_HEAD(readSquare(state->board, curPos)).stone != FLAT) {
                return INVALID_SLIDE;
            } 
            else if (curPos == endPos) {
                if (mv->crush == CRUSH) {
                    Square* sq = readSquare(state->board, curPos);
                    if (sq->numPieces < 2) return INVALID_CRUSH;
                    if (SQ_HEAD(sq).stone != CAP) return INVALID_CRUSH;
                }
            }
            i++;
            curPos = nextPosition(curPos, mv->direction);
        }
        undoMoveNoChecks(state, move, true);
    } 
    else {
        return INVALID_MOVE_TYPE;
    }
    return SUCCESS;
}

GameState* undoMoveNoChecks(GameState* state, const Move* move, bool doHistory) {
    if (move->type == PLACE) {
        const PlaceMove* mv = &move->move.place;
        Square* sq = readSquare(state->board, mv->pos);
        squareRemovePiece(state, sq);
        /* updateSquareVector(state, mv->pos); */
        Reserves* reserves = (mv->color == WHITE) ? &state->player1 : &state->player2;
        if (mv->stone == FLAT || mv->stone == STANDING) {
            reserves->stones++;
        } else {
            reserves->caps++;
        }
    } 
    else if (move->type == SLIDE) {
        state->hash = clearSlideHash(state->hash, &move->move.slide, state);
        const SlideMove* mv = &move->move.slide;
        u8 slideLength = 0;
        while (slideLength < MAX_DROPS && ((mv->drops >> (slideLength * 3)) & 0x7)) {
            slideLength++;
        }

        Position endPos = slidePosition(mv->startPos, mv->direction, slideLength);
        Position curPos = endPos;
        Direction invDir = oppositeDirection(mv->direction);

        PieceStack combinedStack = {0};
        for (u8 i = 0; i < slideLength; i++) {
            u8 dropCount = (mv->drops >> ((slideLength - i - 1) * 3)) & 0x7;
            if (dropCount == 0) break;

            Square* sq = readSquare(state->board, curPos);
            PieceStack removed = squareRemovePieces(state, sq, dropCount);
            /* updateSquareVector(state, curPos); */


            combinedStack = combineStacks(&removed, &combinedStack);

            curPos = nextPosition(curPos, invDir);
        }

        squareInsertPieces(state, readSquare(state->board, mv->startPos), &combinedStack);
        /* updateSquareVector(state, mv->startPos); */

        if (mv->crush == CRUSH) {
            Square* sq = readSquare(state->board, endPos);
            SQ_HEAD(sq).stone = STANDING;
            state->standingStones |= positionToBit(endPos);
        }
    }

    state->turn = oppositeColor(state->turn);
    state->turnNumber--;
    state->hash = incrementalUpdateHash(state->hash, move, state);
    /* state->hash = computeBoardHash(state); */
    if (doHistory) {
        state->history = removeHead(state->history);
    }
    /* updateBitboards(state); */
    return state;
}

int generateAllMoves(const GameState* state, MoveList* moveList) {
    clearMoveList(moveList);
    if (state->result != CONTINUE) return 1;

    if (state->turnNumber <= 2) {
        u8 totalMoves = 0;
#pragma unroll
        for (u8 j = 0; j < TOTAL_SQUARES; j++) {
            if (state->emptySquares & (1ULL << j)) {
                addMoveToList(moveList, createPlaceMove(j, oppositeColor(state->turn), FLAT));
            }
        }

        return 0;
    }

    const Reserves* res = (state->turn == WHITE) ? &state->player1 : &state->player2;
    Color turn = state->turn;

    u8 controlledPositions[TOTAL_SQUARES];
    u8 numControlled = 0;
    Bitboard control = (state->turn == WHITE) ? 
        state->whiteControlled : 
        state->blackControlled;

#pragma unroll
    for (u8 j = 0; j < TOTAL_SQUARES; j++) {
        if (control & (1ULL << j)) {
            controlledPositions[numControlled++] = j;
        }

        if (state->emptySquares & (1ULL << j)) {
            if (res->stones > 0) {
                addMoveToList(moveList, createPlaceMove(j, turn, FLAT));
                addMoveToList(moveList, createPlaceMove(j, turn, STANDING));
            }
            if (res->caps > 0) {
                addMoveToList(moveList, createPlaceMove(j, turn, CAP));
            }
        }
    }

    const u8 CHUNK_SIZE = 12;
#pragma unroll
    for (u8 chunk = 0; chunk < numControlled; chunk += CHUNK_SIZE) {
#pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < numControlled; i++) {
            Position pos = controlledPositions[i];
            generateSlidesInDir(state, pos, LEFT, moveList);
        }
#pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < numControlled; i++) {
            Position pos = controlledPositions[i];
            generateSlidesInDir(state, pos, RIGHT, moveList);
        }
#pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < numControlled; i++) {
            Position pos = controlledPositions[i];
            generateSlidesInDir(state, pos, UP, moveList);
        }
#pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < numControlled; i++) {
            Position pos = controlledPositions[i];
            generateSlidesInDir(state, pos, DOWN, moveList);
        }
    }

    return 0;
}

int generateSlidesInDir(const GameState* state, Position pos, Direction dir, MoveList* moveList) {
    Square* sq = readSquare(state->board, pos);
    u8 maxCount = (sq->numPieces < MAX_PICKUP) ? sq->numPieces : MAX_PICKUP;
    Color turn = state->turn;
    Board* board = state->board;

    u8 steps = numSteps(state, pos, dir);
    if (steps != 0) {
        for (u8 curCount = 1; curCount <= maxCount; curCount++) {
            u32 numberOfSlides = COUNT_VAL_SEQ(curCount, steps);
            const u16* sequences = DROP_SEQUENCE(curCount, steps);
            for (u8 i = 0; i < numberOfSlides; i++) {
                addMoveToList(moveList, 
                    createSlideMove(turn, pos, dir, curCount, sequences[i], NO_CRUSH));
            }
        }
    }

    // Determine if we have a crush situation
    Position crushPos = slidePosition(pos, dir, steps + 1);
    if (!VALID_POSITION(crushPos)) return 0;
    Square* crushSq = readSquare(board, crushPos);
    Crush canCrush = 
        (SQ_HEAD(sq).stone == CAP) && 
        (VALID_POSITION(crushPos)) && (steps + 1 <= maxCount) &&
        (crushSq->numPieces > 0) && 
        (SQ_HEAD(crushSq).stone == STANDING) 
        ? CRUSH : NO_CRUSH;

    if (canCrush == CRUSH) {
        if (steps == 0 || maxCount == 1) {
            addMoveToList(moveList, createSlideMove(turn, pos, dir, 1, 1, CRUSH));
            return 0;
        }
        for (u8 curCount = steps + 1; curCount <= maxCount; curCount++) {
            const u16* sequences = DROP_SEQUENCE_CRUSH(curCount, steps);
            u8 i = 0;
            while (sequences[i] != 0) {
                addMoveToList(moveList, createSlideMove(turn, pos, dir, curCount, sequences[i], CRUSH));
                i++;
            }
        }
    }
    return 0;
}

#pragma inline
u8 numSteps(const GameState* state, Position pos, Direction dir) {
    u8 steps = 0;
    pos = nextPosition(pos, dir);
    if (!VALID_POSITION(pos)) return steps;
    Bitboard posBit = 1ULL << pos;

    for (u8 i = 0; i < MAX_DROPS; i++) {
        if (!VALID_POSITION(pos)) return steps;
        if ((state->standingStones | state->capstones) & posBit) {
            return steps;
        }
        pos = nextPosition(pos, dir);
        switch (dir) {
            case UP:
                posBit <<= BOARD_SIZE;
                break;
            case DOWN:
                posBit >>= BOARD_SIZE;
                break;
            case RIGHT:
                posBit <<= 1;
                break;
            case LEFT:
                posBit >>= 1;
                break;
        }
        steps++;
    }
    return steps;
}

int countAllSlidesInDir(const GameState* state, Position pos, Direction dir) {
    int toReturn = 0;
    Square* sq = readSquare(state->board, pos);
    u8 maxCount = (sq->numPieces < MAX_PICKUP) ? sq->numPieces : MAX_PICKUP;
    Color turn = state->turn;
    Board* board = state->board;

    u8 steps = numSteps(state, pos, dir);
    if (steps != 0) {
        for (u8 curCount = 1; curCount <= maxCount; curCount++) {
            u32 numberOfSlides = COUNT_VAL_SEQ(curCount, steps);
            toReturn += numberOfSlides;
        }
    }

    // Determine if we have a crush situation
    Position crushPos = slidePosition(pos, dir, steps + 1);
    if (!VALID_POSITION(crushPos)) return toReturn;
    Square* crushSq = readSquare(board, crushPos);
    Crush canCrush = 
        (SQ_HEAD(sq).stone == CAP) && 
        (VALID_POSITION(crushPos)) && (steps + 1 <= maxCount) &&
        (crushSq->numPieces > 0) && 
        (SQ_HEAD(crushSq).stone == STANDING) 
        ? CRUSH : NO_CRUSH;

    if (canCrush == CRUSH) {
        if (steps == 0 || maxCount == 1) {
            return toReturn + 1;
        }
        for (u8 curCount = steps + 1; curCount <= maxCount; curCount++) {
            toReturn += binCoe[(curCount-1) * 6 + steps];
        }
    }
    return toReturn;
}

int countAllMoves(const GameState* state) {
    if (state->result != CONTINUE) return 0;
    if (state->turnNumber <= 2) {
        return 37 - state->turnNumber;
    }

    u32 totalMoves = 0;
    const Reserves* res = (state->turn == WHITE) ? &state->player1 : &state->player2;
    Color turn = state->turn;

    Bitboard control = (state->turn == WHITE) ? 
        state->whiteControlled : 
        state->blackControlled;

    int sCount = 0;
    if (res->stones > 0) {
        sCount += 2;
    }
    if (res->caps > 0) {
        sCount++;
    }
    totalMoves += __builtin_popcountll(state->emptySquares) * sCount;

    u8 controlledPositions[TOTAL_SQUARES];
    u8 numControlled = 0;
#pragma unroll
    for (u8 j = 0; j < TOTAL_SQUARES; j++) {
        if (control & (1ULL << j)) {
            controlledPositions[numControlled++] = j;
        }
    }
    const u8 CHUNK_SIZE = 12;
#pragma unroll
    for (u8 chunk = 0; chunk < numControlled; chunk += CHUNK_SIZE) {
#pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < numControlled; i++) {
            Position pos = controlledPositions[i];
            totalMoves += countAllSlidesInDir(state, pos, LEFT);
        }
#pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < numControlled; i++) {
            Position pos = controlledPositions[i];
            totalMoves += countAllSlidesInDir(state, pos, RIGHT);
        }
#pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < numControlled; i++) {
            Position pos = controlledPositions[i];
            totalMoves += countAllSlidesInDir(state, pos, UP);
        }
#pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < numControlled; i++) {
            Position pos = controlledPositions[i];
            totalMoves += countAllSlidesInDir(state, pos, DOWN);
        }
    }

    return totalMoves;
}

MoveList* createMoveList(u32 numMoves) {
    MoveList* moveList = malloc(sizeof(MoveList));
    if (!moveList) {
        printf("Memory allocation failed for move list\n");
        return NULL;
    }
    moveList->moves = malloc(numMoves * sizeof(Move));
    if (!moveList->moves) {
        printf("Memory allocation failed for moves array\n");
        free(moveList);
        return NULL;
    }
    moveList->numMoves = 0;
    moveList->capacity = numMoves;
    return moveList;
}

void freeMoveList(MoveList* moves) {
    if (moves) {
        free(moves->moves);
        free(moves);
    }
}

inline void addMoveToList(MoveList* moveList, Move move) {
    if (__builtin_expect(moveList->numMoves < moveList->capacity, 1)) {
        moveList->moves[moveList->numMoves++] = move;
    } else {
        Move* newMoves = realloc(moveList->moves, (moveList->capacity * 2) * sizeof(Move));
        if (newMoves) {
            moveList->moves = newMoves;
            moveList->capacity *= 2;
            moveList->moves[moveList->numMoves++] = move;
        } else {
            printf("Memory allocation failed for moves array\n");
        }
    }
}

inline void clearMoveList(MoveList* moveList) {
    moveList->numMoves = 0;
}

