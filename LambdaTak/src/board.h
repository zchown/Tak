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

typedef enum {FLAT, STANDING, CAP} Stone;
typedef enum {WHITE, BLACK} Color;

typedef struct Piece {
    Stone stone;
    Color color;
    struct Piece* next;
} Piece;

typedef struct {
    u8 x;
    u8 y;
} Position;

typedef struct {
    Piece* head;
    u8 numPieces;
} Square;

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
    Position startPos;
    Direction direction;
    // 3 bits for each drop, 5 drops max = 15 bits + 1 extra
    // Least significant bit is the first drop
    u16 drops;
    u8 count;
    Color color;
    Crush crush;
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
    Board* board;
    Color turn;
    u32 turnNumber;
    Reserves player1;
    Reserves player2;
    Result result;
    GameHistory* history;
    Bitboard whiteControlled;
    Bitboard blackControlled;
    Bitboard emptySquares;
} GameState;


// Game state management
GameState* createGameState();
void freeGameState(GameState* state);
GameState* copyGameState(const GameState* state);

// Board management
Board* createEmptyBoard();
void freeBoard(Board* board);
Board* copyBoard(const Board* board);

// History management
GameHistory* addHistory(GameHistory* history, Move move);
GameHistory* copyHistory(const GameHistory* history);
GameHistory* removeHead(GameHistory* history);
void freeHistory(GameHistory* history);

// Piece management
Piece* createPiece(Stone stone, Color color);
void freePieceStack(Piece* piece);
Piece* copyPieceStack(const Piece* top);

// Square operations
Square createSquare();
void freeSquare(Square* square);
Square squareCopy(const Square* square);
Piece* squareInsertPiece(GameState* state, Square* square, Piece* piece);
Piece* squareInsertPieces(GameState* state, Square* square, Piece* piece, u8 numPieces);
Piece* squareRemovePiece(GameState* state, Square* square);
Piece* squareRemovePieces(GameState* state, Square* square, u8 numPieces);
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
bool isValidPosition(Position pos);

// Bitboard operations
Bitboard positionToBit(Position pos);
Position bitToPosition(Bitboard bit);
void updateBitboards(GameState* state);

// Check for road, flat wins, or draws
Result checkGameResult(const GameState* state);

// Utility functions
bool squareIsEmpty(Square* square);
// No bounds checking
u32 positionToIndex(Position pos);
// No bounds checking
Position indexToPosition(u32 index);
bool isValidPosition(Position pos);
Color oppositeColor(Color color);
Direction oppositeDirection(Direction dir);
// Returns original position if out of bounds
Position nextPosition(Position pos, Direction dir);
// Returns original position if out of bounds
Position slidePosition(Position pos, Direction dir, u8 count);
Position* getNeighbors(Position pos);
// updates reserves in place based on pieces on the board
void updateReserves(GameState* state);
int* controlledSquares(const GameState* state, Color color);
int* emptySquares(const GameState* state);

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

