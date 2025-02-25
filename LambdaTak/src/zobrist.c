#include "board.h"
#include <stdlib.h>
#include <time.h>

ZobristKey zobristTable[TOTAL_SQUARES][NUM_COLORS][NUM_PIECE_TYPES][ZOBRIST_STACK_DEPTH];

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
        for (int i = 0; i < square->numPieces && i < ZOBRIST_STACK_DEPTH; i++) {
            int j = square->numPieces - i - 1;
            const Piece* piece = &square->pieces[j];
            hash ^= zobristTable[pos][piece->color][piece->stone][j];
        }
    }

    // Hash the current player to move
    if (state->turn == BLACK) {
        hash ^= zobristTable[0][0][0][ZOBRIST_STACK_DEPTH - 1];
    }

    return hash;
}

ZobristKey clearSlideHash(ZobristKey hash, const SlideMove* move, const GameState* state) {
    Position pos = move->startPos;
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
        for (u8 j = 0; j < nextCount && j < ZOBRIST_STACK_DEPTH; j++) {
            int depth = nextCount - j - 1;
            const Piece* p = &nextSq->pieces[depth];
            hash ^= zobristTable[nextPos][p->color][p->stone][depth];
        }

        pos = nextPos;
    }
    return hash;
}

ZobristKey incrementalUpdateHash(ZobristKey hash, const Move* move, const GameState* state) {
    // Toggle the turn first
    hash ^= zobristTable[0][0][0][ZOBRIST_STACK_DEPTH - 1];

    if (move->type == PLACE) {
        const PlaceMove* mv = &move->move.place;
        // XOR the new piece at depth 0
        hash ^= zobristTable[mv->pos][mv->color][mv->stone][0];
    } else if (move->type == SLIDE) {
        const SlideMove* mv = &move->move.slide;
        Square* startSq = readSquare(state->board, mv->startPos);
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

            // Remove old contributions from the next square
            for (u8 j = 0; j < nextCount && j < ZOBRIST_STACK_DEPTH; j++) {
                int depth = nextCount - j - 1;
                const Piece* p = &nextSq->pieces[depth];
                hash ^= zobristTable[nextPos][p->color][p->stone][depth];
            }

            // Add new contributions after removal
            for (u8 j = 0; j < nextCount && j < ZOBRIST_STACK_DEPTH; j++) {
                int depth = nextCount - j;
                const Piece* p = &nextSq->pieces[depth];
                hash ^= zobristTable[nextPos][p->color][p->stone][depth];
            }

            pos = nextPos;
        }
    }
    return hash;
}
