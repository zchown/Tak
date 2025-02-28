#ifndef BOARD_H 
#define BOARD_H

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define u8 int_fast8_t
#define u16 uint16_t // I need it to be exactly 16 bits so fast isn't good
#define u32 uint_fast32_t
#define u64 uint_fast64_t

#define BOARD_SIZE 6
#define TOTAL_SQUARES (BOARD_SIZE * BOARD_SIZE)
#define MAX_PICKUP BOARD_SIZE
#define MAX_DROPS (MAX_PICKUP - 1)

#define STONES_PER_PLAYER 30
#define CAPS_PER_PLAYER 1

// precomputed bitboards from sorcery
#define ROW1 0x000000000000003F // top edge
#define ROW2 0x0000000000000FC0
#define ROW3 0x000000000003F000
#define ROW4 0x0000000000FC0000
#define ROW5 0x000000003F000000
#define ROW6 0x0000000FC0000000 // bottom edge

#define COLA  0x0000000041041041 // left edge
#define COLB  0x0000000082082082
#define COLC  0x0000000104104104
#define COLD  0x0000000208208208
#define COLE  0x0000000410410410
#define COLF  0x0000000820820820 // right edge

typedef enum {FLAT, STANDING, CAP} Stone;
typedef enum {WHITE, BLACK} Color;

typedef u64 ZobristKey;

#define NUM_PIECE_TYPES 3  // FLAT, STANDING, CAP
#define NUM_COLORS 2       // WHITE, BLACK
#define ZOBRIST_STACK_DEPTH 7

extern ZobristKey zobristTable[TOTAL_SQUARES][NUM_COLORS][NUM_PIECE_TYPES][ZOBRIST_STACK_DEPTH];

extern ZobristKey zobristTurn;

typedef u8 Position;

#define SET_POS(x, y) ((x) + (y) * BOARD_SIZE)
#define GET_X(pos) ((pos) % BOARD_SIZE)
#define GET_Y(pos) ((pos) / BOARD_SIZE)
#define VALID_POSITION(pos) ((pos) >= 0 && (pos) < TOTAL_SQUARES)
#define RIGHT_POSITION(x) (((x) % BOARD_SIZE == BOARD_SIZE - 1) ? 100 : (x) + 1)
#define LEFT_POSITION(x) (((x) % BOARD_SIZE == 0) ? 100 : (x) - 1)
#define UP_POSITION(y) ((y) + 6 >= TOTAL_SQUARES ? 100 : (y) + 6)
#define DOWN_POSITION(y) ((y) - 6 < 0 ? 100 : (y) - 6)
#define RIGHT_SLIDE_POSITION(x, count) (((x) % BOARD_SIZE + count >= BOARD_SIZE) ? 100 : (x) + count)
#define LEFT_SLIDE_POSITION(x, count) (((x) % BOARD_SIZE - count < 0) ? 100 : (x) - count)
#define UP_SLIDE_POSITION(y, count) ((y) + count * BOARD_SIZE >= TOTAL_SQUARES ? 100 : (y) + count * BOARD_SIZE)
#define DOWN_SLIDE_POSITION(y, count) ((y) - count * BOARD_SIZE < 0 ? 100 : (y) - count * BOARD_SIZE)

typedef struct Piece {
    Stone stone;
    Color color;
} Piece;

typedef struct PieceStack {
    Piece pieces[MAX_PICKUP];
    u8 numPieces;
} PieceStack;

typedef struct {
    Piece pieces[64];
    u8 numPieces;
    u8 whiteStones;
    u8 blackStones;
} Square;

#define SQ_HEAD(square) ((square)->pieces[square->numPieces - 1])

typedef struct {
    Square squares[TOTAL_SQUARES];
} Board;

typedef u64 Bitboard;

typedef struct {
    u8 stones;
    u8 caps;
} Reserves;

typedef enum {ROAD_WHITE, ROAD_BLACK, FLAT_WHITE, FLAT_BLACK, DRAW, CONTINUE} Result;
typedef enum {LEFT, RIGHT, UP, DOWN} Direction;
typedef enum {CRUSH, NO_CRUSH} Crush;
typedef enum {PLACE, SLIDE} MoveType;
typedef enum {VERTICAL, HORIZONTAL} SearchDirection;

typedef struct {
    Position pos;
    Color color;
    Stone stone;
} PlaceMove;

typedef struct {
    Direction direction;
    // 3 bits for each drop, 5 drops max = 15 bits + 1 extra
    // Least significant bit is the first drop
    Crush crush;
    Color color;
    u16 drops;
    u8 count;
    Position startPos;
    } SlideMove;

typedef struct {
    MoveType type;
    union {
        PlaceMove place;
        SlideMove slide;
    } move;
} Move;

typedef struct GameHistory {
    Move move;
    struct GameHistory* next;
} GameHistory;

typedef struct {
    GameHistory* history;
    Reserves player1;
    Reserves player2;
    u32 turnNumber;
    Color turn;
    Result result;
    Board* board;
    Bitboard whiteControlled;
    Bitboard blackControlled;
    Bitboard emptySquares;
    Bitboard standingStones;
    Bitboard capstones;
    ZobristKey hash;
} GameState;

static const u32 TRANSPOSITION_TABLE_SIZE = (1 << 29);

typedef enum {UNDER, OVER, EXACT} EstimationType;

typedef struct {
    Move move;
    int score;
    int depth;
    EstimationType type;
    ZobristKey hash;
} TranspositionEntry;

extern TranspositionEntry* transpositionTable;

void initZobristTable(void);
ZobristKey computeBoardHash(const GameState* state);
ZobristKey clearSlideHash(ZobristKey hash, const SlideMove* move, const GameState* state);
ZobristKey incrementalUpdateHash(ZobristKey hash, const Move* move, const GameState* state);

// Game state management
GameState* createGameState(void);
void freeGameState(GameState* state);
GameState* copyGameState(const GameState* state);

// Board management
Board* createEmptyBoard(void);
void freeBoard(Board* board);
Board* copyBoard(const Board* board);

// History management
GameHistory* addHistory(GameHistory* history, Move move);
GameHistory* copyHistory(const GameHistory* history);
GameHistory* removeHead(GameHistory* history);
void freeHistory(GameHistory* history);

// Piece operations
PieceStack combineStacks(PieceStack* stack1, PieceStack* stack2);

// Square operations
Square createSquare(void);
Square squareCopy(const Square* square);
Piece* squareInsertPiece(GameState* state, Square* square, Piece piece);
Piece* squareInsertPieces(GameState* state, Square* square, PieceStack* stack);
Piece* squareRemovePiece(GameState* state, Square* square);
PieceStack squareRemovePieces(GameState* state, Square* square, u8 numPieces);
bool squareIsEmpty(Square* square);

// Move operations
Move createPlaceMove(Position pos, Color color, Stone stone);
Move createSlideMove(Color color, Position startPos, Direction direction, u8 count, u16 drops, Crush crush);
void freeMove(Move* move);
Move* copyMove(const Move* move);
/* Move* parseMove(const char* moveStr, Color color); */
char* moveToString(const Move* move);

// Board operations
Square* readSquare(const Board* board, Position pos);

// Bitboard operations
Bitboard positionToBit(Position pos);
Position bitToPosition(Bitboard bit);
void updateBitboards(GameState* state);

// Check for road, flat wins, or draws
Result checkGameResult(const GameState* state);

// Utility functions
bool squareIsEmpty(Square* square);
Color oppositeColor(Color color);
Direction oppositeDirection(Direction dir);
// Returns original position if out of bounds
Position nextPosition(Position pos, Direction dir);
// Returns original position if out of bounds
Position slidePosition(Position pos, Direction dir, u8 count);
// updates reserves in place based on pieces on the board
void updateReserves(GameState* state);
int* controlledSquares(const GameState* state, Color color);
int* emptySquares(const GameState* state);
bool movesEqual(const Move* a, const Move* b);

// Print functions
void printMove(const Move* move);
void printHistory(const GameHistory* history);
void printPosition(const Position* pos);
void printPiece(const Piece* piece);
void printSquare(const Square* square);
void printBoard(const Board* board);
void printGameState(const GameState* state);
void printBitboard(Bitboard bitboard);

#endif // BOARD_H

