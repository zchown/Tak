#include "board.h"
#include <stdlib.h>
#include <time.h>

ZobristKey zobristTable[TOTAL_SQUARES][NUM_COLORS][NUM_PIECE_TYPES][ZOBRIST_STACK_DEPTH];
ZobristKey zobristTurn;

TranspositionEntry* transpositionTable;

// Initialize the Zobrist table with random 64-bit values
void initZobristTable(void) {
    printf("Initializing Zobrist table\n");
    // Seed the random number generator
    srand(time(NULL));

    for (int pos = 0; pos < TOTAL_SQUARES; pos++) {
        for (int color = 0; color < NUM_COLORS; color++) {
            for (int type = 0; type < NUM_PIECE_TYPES; type++) {
                for (int depth = 0; depth < ZOBRIST_STACK_DEPTH; depth++) {
                    zobristTable[pos][color][type][depth] = 
                        ((u64)rand() << 32) | ((u64)rand() & 0xFFFFFFFF);
                }
            }
        }
    }
    zobristTurn = ((u64)rand() << 32) | ((u64)rand() & 0xFFFFFFFF);

    transpositionTable = (TranspositionEntry*)malloc(sizeof(TranspositionEntry) * TRANSPOSITION_TABLE_SIZE);
}

// Compute a Zobrist hash for the entire board
ZobristKey computeBoardHash(const GameState* state) {
    if (!state || !state->board) {
        printf("computeBoardHash: Invalid state or board\n");
        return 0;
    }

    ZobristKey hash = 0;

    // Hash each piece on the board
    for (int pos = 0; pos < TOTAL_SQUARES; pos++) {
        const Square* square = &state->board->squares[pos];

        // Hash up to ZOBRIST_STACK_DEPTH pieces in each stack
        int i = (square->numPieces - ZOBRIST_STACK_DEPTH) < 0 ? 0 : square->numPieces - ZOBRIST_STACK_DEPTH;
        for (;i < square->numPieces; i++) {
            const Piece* piece = &square->pieces[i];
            hash ^= zobristTable[pos][piece->color][piece->stone][i];
        }
    }

    // Hash the current player to move
    if (state->turn == BLACK) {
        hash ^= zobristTurn;
    }

    return hash;
}

ZobristKey clearSlideHash(ZobristKey hash, const SlideMove* move, const GameState* state) {
    Position pos = move->startPos;

    Square* startSq = readSquare(state->board, pos);
    int j = (startSq->numPieces - ZOBRIST_STACK_DEPTH) < 0 ? 0 : startSq->numPieces - ZOBRIST_STACK_DEPTH;
    for (; j < startSq->numPieces; j++) {
        const Piece* p = &startSq->pieces[j];
        hash ^= zobristTable[pos][p->color][p->stone][j];
    }

    u8 slideLength = 0;
    while (slideLength < MAX_DROPS && ((move->drops >> (slideLength * 3)) & 0x7)) {
        slideLength++;
    }

    for (int i = 0; i < slideLength; i++) {
        Direction dir = move->direction;
        Position nextPos = nextPosition(pos, dir);
        Square* nextSq = readSquare(state->board, nextPos);
        u8 nextCount = nextSq->numPieces;

        // Remove old contributions from the next square
        int j = (nextCount - ZOBRIST_STACK_DEPTH) < 0 ? 0 : nextCount - ZOBRIST_STACK_DEPTH;
        for (; j < nextCount; j++) {
            const Piece* p = &nextSq->pieces[j];
            hash ^= zobristTable[nextPos][p->color][p->stone][j];
        }
        pos = nextPos;
    }
    return hash;
}

ZobristKey incrementalUpdateHash(ZobristKey hash, const Move* move, const GameState* state) {
    // Toggle the turn first
    hash ^= zobristTurn;

    if (move->type == PLACE) {
        const PlaceMove* mv = &move->move.place;
        // XOR the new piece at depth 0
        hash ^= zobristTable[mv->pos][mv->color][mv->stone][0];
    } else if (move->type == SLIDE) {
        const SlideMove* mv = &move->move.slide;
        Square* startSq = readSquare(state->board, mv->startPos);

        int j = (startSq->numPieces - ZOBRIST_STACK_DEPTH) < 0 ? 0 : startSq->numPieces - ZOBRIST_STACK_DEPTH;
        for (; j < startSq->numPieces; j++) {
            const Piece* p = &startSq->pieces[j];
            hash ^= zobristTable[mv->startPos][p->color][p->stone][j];
        }


        u8 startCount = startSq->numPieces;

        // Add new contributions after removal
        Position pos = mv->startPos;
        u8 slideLength = 0;
        while (slideLength < MAX_DROPS && ((mv->drops >> (slideLength * 3)) & 0x7)) {
            slideLength++;
        }

        for (int i = 0; i < slideLength; i++) {
            Direction dir = mv->direction;
            Position nextPos = nextPosition(pos, dir);
            Square* nextSq = readSquare(state->board, nextPos);
            u8 nextCount = nextSq->numPieces;

            // Add new contributions after removal
            int j = (nextCount - ZOBRIST_STACK_DEPTH) < 0 ? 0 : nextCount - ZOBRIST_STACK_DEPTH;
            for (; j < nextCount; j++) {
                const Piece* p = &nextSq->pieces[j];
                hash ^= zobristTable[nextPos][p->color][p->stone][j];
            }

            pos = nextPos;
        }
    }
    return hash;
}
