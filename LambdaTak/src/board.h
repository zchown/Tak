#ifndef BOARD_H 
#define BOARD_H

#include <stdint.h>
#include <stdbool.h>

#define u8 uint8_t
#define u64 uint64_t

#define BOARD_SIZE 6
#define TOTAL_SQUARES (BOARD_SIZE * BOARD_SIZE)
#define MAX_PICKUP BOARD_SIZE

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
    Position pos;
    u8 numPieces;
} Square;

typedef struct {
    Square squares[TOTAL_SQUARES];
} Board;

typedef struct {
    u8 stones;
    u8 caps;
} Reserves;

typedef enum {ROAD_WHITE, ROAD_BLACK, FLAT_WHITE, FLAT_BLACK, DRAW, CONTINUE} Result;
typedef enum {LEFT, RIGHT, UP, DOWN} Direction;
typedef enum {CRUSH, NO_CRUSH} Crush;

typedef struct {
    Position pos;
    Color color;
    Stone stone;
} PlaceMove;

typedef struct {
    Position startPos;
    u8 count;
    Direction direction;
    u8 drops[MAX_PICKUP];
    Crush crush;
} SlideMove;

typedef union {
    PlaceMove place;
    SlideMove slide;
} Move;

typedef struct GameHistory {
    Move move;
    struct GameHistory* next;
} GameHistory;

typedef struct {
    Board* board;
    Color turn;
    u64 turnNumber;
    Reserves player1;
    Reserves player2;
    Result result;
    GameHistory* history;
} GameState;

// Game state management
GameState* createGameState();
void freeGameState(GameState* state);
GameState* copyGameState(const GameState* state);

// Board management
Board* createEmptyBoard();
void freeBoard(Board* board);
Board* copyBoard(const Board* board);

// Piece management
Piece* createPiece(Stone stone, Color color);
void freePieceStack(Piece* piece);
Piece* copyPieceStack(Piece* top);

// General Helpers
Color flipColor(Color color);

// Square operations
Square* createSquare(Position pos);
void freeSquare(Square* square);
Square* squareCopy(const Square* square);

Piece* squareInsertPiece(Square* square, Piece* piece);
Piece* squareHead(Square* square);
Piece* squareRemovePiece(Square* square);
Piece* squareRemovePieces(Square* square, u8 numPieces);
bool squareIsEmpty(Square* square);

// Position conversions
u64 positionToIndex(Position pos);
Position indexToPosition(u64 index);

// Board operations
Square* readSquare(Board* board, Position pos);
bool isValidPosition(Position pos);

// Move execution
bool applyMove(GameState* state, Move move);
bool placeStone(GameState* state, PlaceMove move);
bool slideStack(GameState* state, SlideMove move);

// Game rules & validation
bool isLegalMove(const GameState* state, Move move);
bool canCrush(Piece* stackTop);
Result checkGameResult(GameState* state);

// Utility functions
void printMove(const Move* move);
void printPosition(const Position* pos);
void printPiece(const Piece* piece);
void printSquare(const Square* square);
void printBoard(const Board* board);
void printGameState(const GameState* state);

#endif // BOARD_H

