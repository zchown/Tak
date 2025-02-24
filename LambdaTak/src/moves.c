#include "moves.h"
#include "magic.h"

MoveResult checkMove(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        if (!VALID_POSITION(move->move.place.pos)) return INVALID_POSITION;
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
        if (!VALID_POSITION(move->move.slide.startPos)) return INVALID_POSITION;
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
        PlaceMove* mv = &move->move.place;
        Square* sq = readSquare(state->board, mv->pos);
        Piece* piece = createPiece(mv->stone, mv->color);
        squareInsertPiece(state, sq, piece);
        Reserves* reserves = (mv->color == WHITE) ? &state->player1 : &state->player2;
        if (mv->stone == FLAT || mv->stone == STANDING) {
            reserves->stones--;
        }
        else {
            reserves->caps--;
        }
    } 
    else if (move->type == SLIDE) {
        SlideMove* mv = &move->move.slide;
        Piece* pieces = squareRemovePieces(state, readSquare(state->board, mv->startPos), mv->count);

        u8 slideLength = 1;
        while (((mv->drops >> (slideLength * 3)) & 0x7) != 0) {
            slideLength++;
        }

        Position pos = slidePosition(mv->startPos, mv->direction, slideLength);
        if (mv->crush == CRUSH) {
            Square* sq = readSquare(state->board, pos);
            if (sq->head) {
                sq->head->stone = FLAT;
                state->standingStones &= ~(1ULL << pos);
            }
        }

        Direction dir = oppositeDirection(mv->direction);
        for (u8 i = 0; i < slideLength; i++) {
            Square* sq = readSquare(state->board, pos);
            u8 dropCount = (mv->drops >> ((slideLength - i - 1) * 3)) & 0x7;
            pieces = squareInsertPieces(state, sq, pieces, dropCount);
            pos = nextPosition(pos, dir);
        }
    }

    state->turn = (state->turn == WHITE) ? BLACK : WHITE;
    state->turnNumber++;
    if (doHistory) {
        state->history = addHistory(state->history, *move);
    }
    return state;
}

MoveResult undoMoveChecks(GameState* state, const Move* move) {
    if (move->type == PLACE) {
        if (!VALID_POSITION(move->move.place.pos)) return INVALID_POSITION;
        if (readSquare(state->board, move->move.place.pos)->head == NULL) return INVALID_POSITION;
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

            if (curPos != endPos && readSquare(state->board, curPos)->head->stone != FLAT) {
                return INVALID_SLIDE;
            } 
            else if (curPos == endPos) {
                if (mv->crush == CRUSH) {
                    if (readSquare(state->board, curPos)->numPieces < 2) return INVALID_CRUSH;
                    if (readSquare(state->board, curPos)->head->stone != CAP) return INVALID_CRUSH;
                    if (readSquare(state->board, curPos)->head->next->stone == CAP) return INVALID_CRUSH;
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
        PlaceMove* mv = &move->move.place;
        Square* sq = readSquare(state->board, mv->pos);
        squareRemovePiece(state, sq);
        Reserves* reserves = (mv->color == WHITE) ? &state->player1 : &state->player2;
        if (mv->stone == FLAT || mv->stone == STANDING) {
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
        while (curPos != mv->startPos) {
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

        if(mv->crush == CRUSH) {
            Square* sq = readSquare(state->board, endPos);
            sq->head->stone = STANDING;
            state->standingStones |= 1ULL << endPos;
        }
    }

    state->turn = (state->turn == WHITE) ? BLACK : WHITE;
    state->turnNumber--;
    if (doHistory) {
        state->history = removeHead(state->history);
    }
    return state;
}

GeneratedMoves* generateAllMoves(const GameState* state) {
    if (state->result != CONTINUE) return NULL;
    GeneratedMoves* toReturn = malloc(sizeof(GeneratedMoves));
    if (state->turnNumber <= 2) {
        Move* moves = malloc(36 * sizeof(Move));
        if (!moves) {
            printf("Memory allocation failed for moves array\n");
            free(toReturn);
            return NULL;
        }

        u8 totalMoves = 0;
        for (u8 j = 0; j < TOTAL_SQUARES; j++) {
            if (state->emptySquares & (1ULL << j)) {
                moves[totalMoves++] = createPlaceMove(j, oppositeColor(state->turn), FLAT);
            }
        }

        toReturn->moves = moves;
        toReturn->numMoves = totalMoves;
        return toReturn;
    }

    // https://theses.liacs.nl/pdf/LaurensBeljaards2017Tak.pdf
    // hope this is big enough (certainly for any reasonable position it is)
    Move* moves = malloc(512 * sizeof(Move));
    if (!moves) {
        printf("Memory allocation failed for moves array\n");
        free(toReturn);
        return NULL;
    }

    u32 totalMoves = 0;
    Reserves* res = (state->turn == WHITE) ? &state->player1 : &state->player2;
    Color turn = state->turn;

    u8 controlled_positions[TOTAL_SQUARES];
    u8 num_controlled = 0;
    Bitboard control = (state->turn == WHITE) ? 
        state->whiteControlled : 
        state->blackControlled;

    for (u8 j = 0; j < TOTAL_SQUARES; j++) {
        if (control & (1ULL << j)) {
            controlled_positions[num_controlled++] = j;
        }

        if (state->emptySquares & (1ULL << j)) {
            if (res->stones > 0) {
                moves[totalMoves] = createPlaceMove(j, turn, FLAT);
                totalMoves++;
                moves[totalMoves] = createPlaceMove(j, turn, STANDING);
                totalMoves++;
            }
            if (res->caps > 0) {
                moves[totalMoves] = createPlaceMove(j, turn, CAP);
                totalMoves++;
            }
        }
    }

    const u8 CHUNK_SIZE = 18;
    #pragma unroll
    for (u8 chunk = 0; chunk < num_controlled; chunk += CHUNK_SIZE) {
        #pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < num_controlled; i++) {
            Position pos = controlled_positions[i];
            generateSlidesInDir(state, pos, LEFT, moves, &totalMoves);
        }
        #pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < num_controlled; i++) {
            Position pos = controlled_positions[i];
            generateSlidesInDir(state, pos, RIGHT, moves, &totalMoves);
        }
        #pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < num_controlled; i++) {
            Position pos = controlled_positions[i];
            generateSlidesInDir(state, pos, UP, moves, &totalMoves);
        }
        #pragma unroll
        for (u8 i = chunk; i < chunk + CHUNK_SIZE && i < num_controlled; i++) {
            Position pos = controlled_positions[i];
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
    if (steps != 0) {
        for (u8 curCount = 1; curCount <= maxCount; curCount++) {
            u32 numberOfSlides = COUNT_VAL_SEQ(curCount, steps);
            u16* sequences = DROP_SEQUENCE(curCount, steps);
            for (u8 i = 0; i < numberOfSlides; i++) {
                moves[*totalMoves] = createSlideMove(turn, pos, dir, curCount, sequences[i], NO_CRUSH);
                (*totalMoves)++;
            }
        }
    }

    // Determine if we have a crush situation
    Position crushPos = slidePosition(pos, dir, steps + 1);
    if (!VALID_POSITION(crushPos)) return;
    Square* crushSq = readSquare(board, crushPos);
    Crush canCrush = 
        (sq->head->stone == CAP) && 
        (VALID_POSITION(crushPos)) && (steps + 1 <= maxCount) &&
        (crushSq->head != NULL) && 
        (crushSq->head->stone == STANDING) 
        ? CRUSH : NO_CRUSH;

    if (canCrush == CRUSH) {
        if (steps == 0 || maxCount == 1) {
            moves[*totalMoves] = createSlideMove(turn, pos, dir, 1, 1, CRUSH);
            (*totalMoves)++;
            return;
        }
        for (u8 curCount = steps + 1; curCount <= maxCount; curCount++) {
            u16* sequences = DROP_SEQUENCE_CRUSH(curCount, steps);
            u8 i = 0;
            while (sequences[i] != 0) {
                moves[*totalMoves] = createSlideMove(turn, pos, dir, curCount, sequences[i], CRUSH);
                (*totalMoves)++;
                i++;
            }
        }
    }
}

#pragma inline
u8 numSteps(const GameState* state, Position pos, Direction dir) {
    u8 steps = 0;
    pos = nextPosition(pos, dir);
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

#pragma inline
void freeGeneratedMoves(GeneratedMoves* moves) {
    if (!moves) return;
    free(moves->moves);
    free(moves);
}
