#include "board.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

GameState* createGameState() {
    GameState* state = (GameState*)malloc(sizeof(GameState));
    if (!state) {
        printf("createGameState: Failed to allocate memory for game state\n");
        return NULL;
    }
    state->board = createEmptyBoard();
    state->turn = WHITE;
    state->turnNumber = 1;
    state->player1 = (Reserves){STONES_PER_PLAYER, CAPS_PER_PLAYER};
    state->player2 = (Reserves){STONES_PER_PLAYER, CAPS_PER_PLAYER};
    state->result = CONTINUE;
    state->history = NULL;

    return state;
}

void freeGameState(GameState* state) {
    if (!state) {
        printf("freeGameState: Game state is NULL\n");
        return;
    }

    freeBoard(state->board);
    free(state);
}

Board* createEmptyBoard() {
    Board* board = (Board*)malloc(sizeof(Board));
    if (!board) {
        printf("createEmptyBoard: Failed to allocate memory for board\n");
        return NULL;
    }

    memset(board, 0, sizeof(Board));
    return board;
}

void freeBoard(Board* board) {
    if (!board) {
        printf("freeBoard: Board is NULL\n");
        return;
    }

    for (int i = 0; i < TOTAL_SQUARES; i++) {
        freeSquare(&board->squares[i]);
    }
}


Piece* createPiece(Stone stone, Color color) {
    Piece* piece = (Piece*)malloc(sizeof(Piece));
    if (!piece) {
        printf("createPiece: Failed to allocate memory for piece\n");
        return NULL;
    }

    piece->stone = stone;
    piece->color = color;
    piece->next = NULL;
    return piece;
}

void freePieceStack(Piece* piece) {
    while (piece) {
        Piece* next = piece->next;
        free(piece);
        piece = next;
    }
}

Piece* copyPieceStack(Piece* top) {
    Piece* newTop = NULL;
    Piece* current = top;
    Piece* prev = NULL;

    while (current) {
        Piece* newPiece = createPiece(current->stone, current->color);
        if (!newPiece) {
            printf("copyPieceStack: Failed to copy piece stack\n");
            freePieceStack(newTop);
            return NULL;
        }

        if (prev) {
            prev->next = newPiece;
        }
        else {
            newTop = newPiece;
        }

        prev = newPiece;
        current = current->next;
    }

    return newTop;
}

Square* createSquare(Position pos) {
    Square* square = (Square*)malloc(sizeof(Square));
    if (!square) {
        printf("createSquare: Failed to allocate memory for square\n");
        return NULL;
    }

    square->head = NULL;
    square->numPieces = 0;
    square->pos = pos;
    return square;
}

void freeSquare(Square* square) {
    if (!square) {
        printf("freeSquare: Square is NULL\n");
        return;
    }

    freePieceStack(square->head);
    free(square);
}


Piece* squareInsertPiece(Square* square, Piece* piece) {
    if (!square) {
        printf("squareInsertPiece: Square is NULL\n");
        return NULL;
    }
    if (!piece) {
        printf("squareInsertPiece: Piece is NULL\n");
        return NULL;
    }

    piece->next = square->head;
    square->head = piece;
    square->numPieces++;
    return piece;
}

Piece* squareHead(Square* square) {
    return square ? square->head : NULL;
}

Piece* squareRemovePiece(Square* square) {
    if (!square) {
        printf("squareRemovePiece: Square is NULL\n");
        return NULL;
    }
    if (!square->head) {
        printf("squareRemovePiece: Square is empty\n");
        return NULL;
    }
    Piece* removed = square->head;
    square->head = removed->next;
    square->numPieces--;
    removed->next = NULL;
    return removed;
}

Piece* squareRemovePieces(Square* square, u8 numPieces) {
    if (!square) {
        printf("squareRemovePieces: Square is NULL\n");
        return NULL;
    }
    if (numPieces == 0) {
        printf("squareRemovePieces: numPieces is 0\n");
        return NULL;
    }
    if (numPieces > square->numPieces) {
        printf("squareRemovePieces: numPieces is greater than the number of pieces in the square\n");
        return NULL;
    }
    Piece* top = square->head;
    Piece* prev = NULL;
    for (u8 i = 0; i < numPieces; i++) {
        prev = top;
        top = top->next;
    }
    if (prev) prev->next = NULL;
    square->head = top;
    square->numPieces -= numPieces;
    return prev;
}

bool squareIsEmpty(Square* square) {
    return square ? square->head == NULL : true;
}

u64 positionToIndex(Position pos) {
    return pos.y * BOARD_SIZE + pos.x;
}

Position indexToPosition(u64 index) {
    Position pos = { index % BOARD_SIZE, index / BOARD_SIZE };
    return pos;
}

Square* readSquare(Board* board, Position pos) {
    if (!board) {
        printf("readSquare: Board is NULL\n");
        return NULL;
    }
    if (!isValidPosition(pos)) {
        printf("readSquare: Invalid position\n");
        return NULL;
    }
    if (pos.x >= BOARD_SIZE || pos.y >= BOARD_SIZE) {
        printf("readSquare: Position out of bounds\n");
        return NULL;
    }
    return &board->squares[positionToIndex(pos)];
}

bool isValidPosition(Position pos) {
    return pos.x < BOARD_SIZE && pos.y < BOARD_SIZE;
}

bool checkReservesEmpty(GameState* state) {
    return (state->player1.stones == 0 && state->player1.caps == 0) ||
           state->player2.stones == 0 && state->player2.caps == 0;
}

typedef enum {VERTICAL, HORIZONTAL} SearchDirection;

Result checkRoadWin(GameState* state) {
    
}

Result checkFullBoard(GameState* state, bool emptyReserves) {
    bool fullBoard = true;
    u8 numWhiteFlats = 0;
    u8 numBlackFlats = 0;

    for (int i = 0; i < TOTAL_SQUARES && (fullBoard || emptyReserves) ; i++) {
        if (state->board->squares[i].numPieces == 0) {
            fullBoard = false;
        }
        else {
            Piece* head = state->board->squares[i].head;
            if (head->stone != STANDING) {
                if (head->color == WHITE) {
                    numWhiteFlats++;
                }
                else {
                    numBlackFlats++;
                }
            }
        }
    }
    if (!fullBoard && !emptyReserves) {
        return CONTINUE;
    }
    else if (numWhiteFlats > numBlackFlats) {
        return FLAT_WHITE;
    }
    else if (numBlackFlats > numWhiteFlats) {
        return FLAT_BLACK;
    }
    else {
        return DRAW;
    }
}

// does not modify game state
Result checkGameResult(GameState* state) {
    // check for road win
    if (checkRoadWin(state) != CONTINUE) {
        return checkRoadWin(state);
    }
    
    // check for reserves empty
    bool reservesFlag = checkReservesEmpty(state);
    
    // either way check for full board / flats win
    return checkFullBoard(state, reservesFlag);
}

void printPiece(const Piece* piece) {
    if (!piece) {
        printf("_");
        return;
    }

    char color = (piece->color == WHITE) ? '1' : '2';
    char stone;

    switch (piece->stone) {
        case FLAT: stone = ' '; break;
        case STANDING: stone = 'S'; break;
        case CAP: stone = 'C'; break;
        default: stone = '?'; break;
    }

    printf("%c%c", color, stone);
}

void printSquare(const Square* square) {
    if (!square || !square->head) {
        printf("_");
        return;
    }

    Piece* current = square->head;
    while (current) {
        printPiece(current);
        current = current->next;
        if (current) printf(" ");
    }
}

void printBoard(const Board* board) {
    if (!board) {
        printf("printBoard: Board is NULL\n");
        return;
    }

    for (int y = BOARD_SIZE - 1; y >= 0; y--) {
        printf("%d |", y + 1);
        for (int x = 0; x < BOARD_SIZE; x++) {
            printf(" ");
            printSquare(&board->squares[y * BOARD_SIZE + x]);
            printf(" |");
        }
        printf("\n");
    }

    printf("    ");
    for (int x = 0; x < BOARD_SIZE; x++) {
        printf("  %c   ", 'a' + x);
    }
    printf("\n");
}

void printGameState(const GameState* state) {
    if (!state) {
        printf("printGameState: Game state is NULL\n");
        return;
    }

    printf("Turn: %s (Turn Number: %llu)\n", 
           (state->turn == WHITE) ? "White" : "Black",
           state->turnNumber);

    printf("Reserves - White: Stones: %d, Caps: %d | Black: Stones: %d, Caps: %d\n",
           state->player1.stones, state->player1.caps,
           state->player2.stones, state->player2.caps);

    printf("Board:\n");
    printBoard(state->board);

    printf("Game Result: ");
    switch (state->result) {
        case ROAD_WHITE: printf("White wins by road\n"); break;
        case ROAD_BLACK: printf("Black wins by road\n"); break;
        case FLAT_WHITE: printf("White wins by flats\n"); break;
        case FLAT_BLACK: printf("Black wins by flats\n"); break;
        case DRAW: printf("Draw\n"); break;
        case CONTINUE: printf("Game in progress\n"); break;
    }
}

