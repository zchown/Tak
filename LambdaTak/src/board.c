#include "board.h"

GameState* createGameState(void) {
    GameState* state = malloc(sizeof(GameState));
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

    state->whiteControlled = 0;
    state->blackControlled = 0;
    state->emptySquares = (1ULL << TOTAL_SQUARES) - 1;
    return state;
}

void freeGameState(GameState* state) {
    if (!state) {
        printf("freeGameState: Game state is NULL\n");
        return;
    }
    freeBoard(state->board);
    freeHistory(state->history);
    free(state);
}

GameState* copyGameState(const GameState* state) {
    GameState* newState = malloc(sizeof(GameState));
    if (!newState) {
        printf("copyGameState: Failed to allocate memory for new game state\n");
        return NULL;
    }
    newState->board = copyBoard(state->board);
    newState->turn = state->turn;
    newState->turnNumber = state->turnNumber;
    newState->player1 = state->player1;
    newState->player2 = state->player2;
    newState->result = state->result;
    newState->history = copyHistory(state->history);

    newState->whiteControlled = state->whiteControlled;
    newState->blackControlled = state->blackControlled;
    newState->emptySquares = state->emptySquares;
    newState->standingStones = state->standingStones;
    newState->capstones = state->capstones;

    return newState;
}

Board* createEmptyBoard(void) {
    Board* board = malloc(sizeof(Board));
    if (!board) {
        printf("createEmptyBoard: Failed to allocate memory for board\n");
        return NULL;
    }
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        board->squares[i] = createSquare();
    }
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
    free(board);
}

Board* copyBoard(const Board* board) {
    Board* newBoard = malloc(sizeof(Board));
    if (!newBoard) {
        printf("copyBoard: Failed to allocate memory for new board\n");
        return NULL;
    }
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        newBoard->squares[i] = squareCopy(&board->squares[i]);
    }
    return newBoard;
}

#pragma inline
GameHistory* addHistory(GameHistory* history, Move move) {
    GameHistory* newHistory = malloc(sizeof(GameHistory));
    if (!newHistory) {
        printf("addHistory: Failed to allocate memory for new history\n");
        return history;
    }
    newHistory->next = history;
    newHistory->move = move;
    return newHistory;
}

GameHistory* copyHistory(const GameHistory* history) {
    GameHistory* newHistory = NULL;
    const GameHistory* current = history;
    while (current) {
        GameHistory* newEntry = malloc(sizeof(GameHistory));
        if (!newEntry) {
            printf("copyHistory: Failed to allocate memory for new history entry\n");
            freeHistory(newHistory);
            return NULL;
        }
        newEntry->move = current->move;
        newEntry->next = newHistory;
        newHistory = newEntry;
        current = current->next;
    }
    return newHistory;
}

#pragma inline
GameHistory* removeHead(GameHistory* history) {
    if (!history) {
        printf("removeHead: History is NULL\n");
        return NULL;
    }
    GameHistory* newHead = history->next;
    free(history);
    return newHead;
}

#pragma inline
void freeHistory(GameHistory* history) {
    while (history) {
        GameHistory* next = history->next;
        free(history);
        history = next;
    }
}

#pragma inline
Square* readSquare(const Board* board, Position pos) {
    if (!board) {
        printf("readSquare: Board is NULL\n");
        return NULL;
    }
    if (!VALID_POSITION(pos)) {
        printf("readSquare: Invalid position\n");
        return NULL;
    }
    return (Square*)&board->squares[pos];
}

Square createSquare(void) {
    Square square;
    square.head = NULL;
    square.numPieces = 0;
    return square;
}

Square squareCopy(const Square* square) {
    Square newSquare;
    newSquare.numPieces = square->numPieces;
    newSquare.head = copyPieceStack(square->head);
    return newSquare;
}

void freeSquare(Square* square) {
    if (!square) {
        printf("freeSquare: Square is NULL\n");
        return;
    }
    freePieceStack(square->head);
    square->head = NULL;
    square->numPieces = 0;
}

#pragma inline
Piece* createPiece(Stone stone, Color color) {
    Piece* piece = malloc(sizeof(Piece));
    if (!piece) {
        printf("createPiece: Failed to allocate memory for piece\n");
        return NULL;
    }
    piece->stone = stone;
    piece->color = color;
    piece->next = NULL;
    return piece;
}

#pragma inline
void freePieceStack(Piece* piece) {
    while (piece) {
        Piece* next = piece->next;
        free(piece);
        piece = next;
    }
}

Piece* copyPieceStack(const Piece* top) {
    Piece* newTop = NULL;
    Piece* prev = NULL;
    const Piece* current = top;
    while (current) {
        Piece* newPiece = createPiece(current->stone, current->color);
        if (!newPiece) {
            printf("copyPieceStack: Failed to allocate memory for piece copy\n");
            freePieceStack(newTop);
            return NULL;
        }
        if (prev) {
            prev->next = newPiece;
        } else {
            newTop = newPiece;
        }
        prev = newPiece;
        current = current->next;
    }
    return newTop;
}

#pragma inline
Piece* squareInsertPiece(GameState* state, Square* square, Piece* piece) {
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
    if (state) {
        Position pos = (square - state->board->squares);
        Bitboard posBit = positionToBit(pos);
        if (piece->color == WHITE) {
            state->whiteControlled |= posBit;
            state->blackControlled &= ~posBit;
        } else {
            state->blackControlled |= posBit;
            state->whiteControlled &= ~posBit;
        }
        if (piece->stone == STANDING) {
            state->standingStones |= posBit;
        } else if (piece->stone == CAP) {
            state->capstones |= posBit;
        }
        state->emptySquares &= ~posBit;
    }
    return piece;
}

#pragma inline
Piece* squareInsertPieces(GameState* state, Square* square, Piece* piece, u8 numPieces) {
    if (!square) {
        printf("squareInsertPieces: Square is NULL\n");
        return NULL;
    }
    if (!piece) {
        printf("squareInsertPieces: Piece is NULL\n");
        return NULL;
    }
    if (numPieces == 0) {
        printf("squareInsertPieces: numPieces is 0\n");
        return NULL;
    }
    Piece* top = piece;
    for (u8 i = 1; i < numPieces; i++) {
        if (!piece) {
            printf("squareInsertPieces: Piece is NULL\n");
            return NULL;
        }
        piece = piece->next;
    } 
    Piece* leftOvers = piece->next;
    piece->next = square->head;
    square->head = top;
    square->numPieces += numPieces;
    if (state) {
        Position pos = (square - state->board->squares);
        Bitboard posBit = positionToBit(pos);
        if (top->color == WHITE) {
            state->whiteControlled |= posBit;
            state->blackControlled &= ~posBit;
            if (top->stone == STANDING) {
                state->standingStones |= posBit;
            } else if (top->stone == CAP) {
                state->capstones |= posBit;
            }
        } else {
            state->blackControlled |= posBit;
            state->whiteControlled &= ~posBit;
            if (top->stone == STANDING) {
                state->standingStones |= posBit;
            } else if (top->stone == CAP) {
                state->capstones |= posBit;
            }
        }
        state->emptySquares &= ~posBit;
    }
    return leftOvers;
}

#pragma inline
Piece* squareRemovePiece(GameState* state, Square* square) {
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
    if (state) {
        Position pos = (square - state->board->squares);
        Bitboard posBit = positionToBit(pos);
        if (square->head) {
            if (square->head->color == WHITE) {
                state->whiteControlled |= posBit;
                state->blackControlled &= ~posBit;
            } else {
                state->blackControlled |= posBit;
                state->whiteControlled &= ~posBit;
            }
        }
        else {
            state->emptySquares |= posBit;
            state->whiteControlled &= ~posBit;
            state->blackControlled &= ~posBit;
        }
        state->standingStones &= ~posBit;
        state->capstones &= ~posBit;
    }
    return removed;
}

#pragma inline
Piece* squareRemovePieces(GameState* state, Square* square, u8 numPieces) {
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
    Piece* toReturn = square->head;
    Piece* top = square->head;
    Piece* prev = NULL;
    for (u8 i = 0; i < numPieces; i++) {
        prev = top;
        top = top->next;
    }
    if (prev) prev->next = NULL;
    square->head = top;
    square->numPieces -= numPieces;
    if (state) {
        Position pos = (square - state->board->squares);
        Bitboard posBit = positionToBit(pos);
        if (square->head) {
            if (square->head->color == WHITE) {
                state->whiteControlled |= posBit;
                state->blackControlled &= ~posBit;
                state->standingStones &= ~posBit;
                state->capstones &= ~posBit;
            } else {
                state->blackControlled |= posBit;
                state->whiteControlled &= ~posBit;
                state->standingStones &= ~posBit;
                state->capstones &= ~posBit;
            }
        }
        else {
            state->emptySquares |= posBit;
            state->whiteControlled &= ~posBit;
            state->blackControlled &= ~posBit;
            state->standingStones &= ~posBit;
            state->capstones &= ~posBit;
        }
    }
    return toReturn;
}

/* #pragma inline */
/* bool squareIsEmpty(Square* square) { */
/*     return (square ? (square->head == NULL) : true); */
/* } */

#pragma inline
Move createPlaceMove(Position pos, Color color, Stone stone) {
    return (Move){PLACE, .move.place = {pos, color, stone}};
}

#pragma inline
Move createSlideMove(Color color, Position startPos, Direction direction, u8 count, u16 drops, Crush crush) {
    return (Move) {SLIDE, .move.slide = {
        .startPos = startPos,
        .color = color,
        .direction = direction,
        .count = count,
        .crush = crush,
        .drops = drops
    }};
}

#pragma inline
void freeMove(Move* move) {
    if (!move) {
        printf("freeMove: Move is NULL\n");
        return;
    }
    free(move);
}

Move* copyMove(const Move* move) {
    if (!move) {
        printf("copyMove: Move is NULL\n");
        return NULL;
    }
    Move* newMove = malloc(sizeof(Move));
    if (!newMove) {
        printf("copyMove: Failed to allocate memory for new move\n");
        return NULL;
    }
    newMove->type = move->type;
    if (move->type == PLACE) {
        newMove->move.place = move->move.place;
    } else {
        newMove->move.slide = move->move.slide;
    }
    return newMove;
}

char* moveToString(const Move* move) {
    if (!move) {
        printf("moveToString: Move is NULL\n");
        return NULL;
    }
    char* moveStr = malloc((5 + MAX_PICKUP) * sizeof(char));
    if (!moveStr) {
        printf("moveToString: Failed to allocate memory for move string\n");
        return NULL;
    }
    u8 size = 0;
    if (move->type == PLACE) {
        if (move->move.place.stone == STANDING)
            moveStr[size++] = 'S';
        else if (move->move.place.stone == CAP)
            moveStr[size++] = 'C';
        moveStr[size++] = 'a' + GET_X(move->move.place.pos);
        moveStr[size++] = '1' + GET_Y(move->move.place.pos);
    } else {
        if (move->move.slide.count > 1)
            moveStr[size++] = '0' + move->move.slide.count;
        moveStr[size++] = 'a' + GET_X(move->move.slide.startPos);
        moveStr[size++] = '1' + GET_Y(move->move.slide.startPos);
        switch (move->move.slide.direction) {
            case LEFT:  moveStr[size++] = '<'; break;
            case RIGHT: moveStr[size++] = '>'; break;
            case UP:    moveStr[size++] = '+'; break;
            case DOWN:  moveStr[size++] = '-'; break;
            default: break;
        }
        u8 curDrop = 0;
        for (int i = 0; i < MAX_DROPS; i++) {
            u8 drop = (move->move.slide.drops >> (i * 3)) & 0x7;
            if (drop == 0) break;
            moveStr[size++] = '0' + drop;
        }
        if (move->move.slide.crush == CRUSH)
            moveStr[size++] = '*';
    }
    moveStr = realloc(moveStr, size + 1);
    moveStr[size] = '\0';
    return moveStr;
}

#pragma inline
Bitboard positionToBit(Position pos) {
    return 1ULL << pos;
}

#pragma inline
Position bitToPosition(Bitboard bit) {
    // Find the index of the least significant 1 bit
    int index = 0;
    while ((bit & 1) == 0) {
        bit >>= 1;
        index++;
    }
    return index;
}

void updateBitboards(GameState* state) {
    if (!state || !state->board) {
        printf("updateBitboards: Invalid state or board\n");
        return;
    }

    // Clear all bitboards
    state->whiteControlled = 0;
    state->blackControlled = 0;
    state->emptySquares = 0;
    state->standingStones = 0;
    state->capstones = 0;

    // Populate bitboards based on current board state
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        Square* sq = &state->board->squares[i];
        Position pos = i;
        Bitboard posBit = positionToBit(pos);

        if (sq->numPieces == 0) {
            state->emptySquares |= posBit;
        }
        else {
            Piece* top = sq->head;
            if (top->color == WHITE) {
                state->whiteControlled |= posBit;
            } else {
                state->blackControlled |= posBit;
            }
            if (top->stone == STANDING) {
                state->standingStones |= posBit;
            } else if (top->stone == CAP) {
                state->capstones |= posBit;
            }
        }
    }
    /* printBitboard(state->whiteControlled); */
}

#pragma inline
bool checkReservesEmpty(const GameState* state) {
    return ((state->player1.stones == 0 && state->player1.caps == 0) ||
            (state->player2.stones == 0 && state->player2.caps == 0));
}

static inline bool hasRoad(Bitboard playerControlled, Bitboard startMask, Bitboard endMask) {
    Bitboard reachable = playerControlled & startMask;
    Bitboard endReachable = playerControlled & endMask;
    if (reachable == 0 || endReachable == 0) {
        return false;
    }

    Bitboard previous;
    do {
        previous = reachable;

        Bitboard shiftedLeft = ((reachable & ~RIGHT_EDGE) << 1) & playerControlled;
        Bitboard shiftedRight = ((reachable & ~LEFT_EDGE) >> 1) & playerControlled;
        Bitboard shiftedUp = ((reachable & ~BOTTOM_EDGE) << BOARD_SIZE) & playerControlled;
        Bitboard shiftedDown = ((reachable & ~TOP_EDGE) >> BOARD_SIZE) & playerControlled;
        reachable |= shiftedLeft | shiftedRight | shiftedUp | shiftedDown;

        if (reachable & endMask) {
            return true;
        }
    } while (reachable != previous);

    return false;
}

Result checkRoadWin(const GameState* state) {
    Color current = state->turn;
    Color opponent = oppositeColor(current);
    Bitboard currentControlled = (current == WHITE) 
        ? (state->whiteControlled & ~state->standingStones)
        : (state->blackControlled & ~state->standingStones);
    Bitboard opponentControlled = (opponent == WHITE)
        ? (state->whiteControlled & ~state->standingStones)
        : (state->blackControlled & ~state->standingStones);

    // Check current player's roads
    if (hasRoad(currentControlled, TOP_EDGE, BOTTOM_EDGE) 
            || hasRoad(currentControlled, LEFT_EDGE, RIGHT_EDGE)) {
        return (current == WHITE) ? ROAD_WHITE : ROAD_BLACK;
    }

    // Check opponent's roads
    if (hasRoad(opponentControlled, TOP_EDGE, BOTTOM_EDGE) 
            || hasRoad(opponentControlled, LEFT_EDGE, RIGHT_EDGE)) {
        return (opponent == WHITE) ? ROAD_WHITE : ROAD_BLACK;
    }

    return CONTINUE;
}

#pragma inline
Result checkFullBoard(const GameState* state, bool emptyReserves) {
    if (state->emptySquares != 0 && !emptyReserves) {
        return CONTINUE;
    }
    Bitboard whiteFlatstones = (state->whiteControlled & ~state->standingStones) & ~state->capstones;
    Bitboard blackFlatstones = (state->blackControlled & ~state->standingStones) & ~state->capstones;

    int diff = __builtin_popcountll(whiteFlatstones) - __builtin_popcountll(blackFlatstones);
    if (diff > 0) {
        return FLAT_WHITE;
    } else if (diff < 0) {
        return FLAT_BLACK;
    } else {
        return DRAW;
    }
}

Result checkGameResult(const GameState* state) {
    Result roadResult = checkRoadWin(state);
    if (roadResult != CONTINUE) {
        return roadResult;
    }
    bool reservesEmpty = checkReservesEmpty(state);
    return checkFullBoard(state, reservesEmpty);
}

#pragma inline
Color oppositeColor(Color color) {
    return (color == WHITE) ? BLACK : WHITE;
}

#pragma inline
Direction oppositeDirection(Direction dir) {
    return (dir == LEFT) ? RIGHT :
        (dir == RIGHT) ? LEFT :
        (dir == UP) ? DOWN : UP;
}

#pragma inline
Position nextPosition(Position pos, Direction dir) {
    switch (dir) {
        case LEFT:
            return LEFT_POSITION(pos);
        case RIGHT:
            return RIGHT_POSITION(pos);
        case UP:
            return UP_POSITION(pos);
        case DOWN:
            return DOWN_POSITION(pos);
    }
    return pos;
}

#pragma inline
Position slidePosition(Position pos, Direction dir, u8 count) {
    switch (dir) {
        case LEFT:
            return LEFT_SLIDE_POSITION(pos, count);
        case RIGHT:
            return RIGHT_SLIDE_POSITION(pos, count);
        case UP:
            return UP_SLIDE_POSITION(pos, count);
        case DOWN:
            return DOWN_SLIDE_POSITION(pos, count);
    }
    return pos;
}

void updateReserves(GameState* state) {
    u8 numWhiteStones = STONES_PER_PLAYER;
    u8 numWhiteCaps = CAPS_PER_PLAYER;
    u8 numBlackStones = STONES_PER_PLAYER;
    u8 numBlackCaps = CAPS_PER_PLAYER;
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        Square* sq = &state->board->squares[i];
        Piece* current = sq->head;
        while (current) {
            /* printf("Color: %d, Stone: %d\n", current->color, current->stone); */
            if (current->color == WHITE) {
                if (current->stone == FLAT)
                    numWhiteStones--;
                else if (current->stone == CAP)
                    numWhiteCaps--;
            } else {
                if (current->stone == FLAT)
                    numBlackStones--;
                else if (current->stone == CAP)
                    numBlackCaps--;
            }
            current = current->next;
        }
    }
    state->player1.stones = numWhiteStones;
    state->player1.caps = numWhiteCaps;
    state->player2.stones = numBlackStones;
    state->player2.caps = numBlackCaps;
    /* printf("White: Stones: %d, Caps: %d | Black: Stones: %d, Caps: %d\n", */
    /*         state->player1.stones, state->player1.caps, */
    /*         state->player2.stones, state->player2.caps); */
}

void printMove(const Move* move) {
    if (!move) {
        printf("printMove: Move is NULL\n");
        return;
    }
    if (move->type == PLACE) {
        printf("Place: %c%d  Player: %c  Stone: %c\n",
                'a' + GET_X(move->move.place.pos),
                GET_Y(move->move.place.pos) + 1,
                (move->move.place.color == WHITE) ? '1' : '2',
                (move->move.place.stone == FLAT) ? 'F' :
                (move->move.place.stone == STANDING) ? 'S' : 'C');
    } else {
        printf("Slide: %c%d  Dir: %c  Crush: %c  Count: %d",
                'a' + GET_X(move->move.slide.startPos),
                GET_Y(move->move.slide.startPos) + 1,
                (move->move.slide.direction == LEFT)  ? '<' :
                (move->move.slide.direction == RIGHT) ? '>' :
                (move->move.slide.direction == UP)    ? '+' : '-',
                (move->move.slide.crush == CRUSH) ? '*' : 'X',
                move->move.slide.count);
        printf("\nColor: %c", (move->move.slide.color == WHITE) ? '1' : '2');
        printf("\nDrops:");
        for (int i = 0; i < MAX_DROPS; i++) {
            u8 drop = (move->move.slide.drops >> (i * 3)) & 0x7;
            if (drop == 0) break;
            printf(" %d", drop);
        }
        printf("\nalt drops: %d", move->move.slide.drops);
        /* for (int i = 0; move->move.slide.drops[i] != 0 && i < MAX_PICKUP; i++) { */
        /*     printf(" %d", move->move.slide.drops[i]); */
        /* } */
        printf("\n");
    }
}

void printHistory(const GameHistory* history) {
    const GameHistory* current = history;
    while (current) {
        printMove(&current->move);
        current = current->next;
    }
}

void printPiece(const Piece* piece) {
    if (!piece) {
        printf("_");
        return;
    }
    char color = (piece->color == WHITE) ? '1' : '2';
    char stone;
    switch (piece->stone) {
        case FLAT: stone = '-'; break;
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
        if (current)
            printf(" ");
    }
}

void printBoard(const Board* board) {
    if (!board) {
        printf("printBoard: Board is NULL\n");
        return;
    }
    printf("\n");
    for (int y = BOARD_SIZE - 1; y >= 0; y--) {
        printf("%d |", y + 1);
        for (int x = 0; x < BOARD_SIZE; x++) {
            Position pos = SET_POS(x, y);
            Square* sq = readSquare(board, pos);
            printf(" ");
            printSquare(sq);
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
    printf("\nTurn: %s (Turn Number: %llu)\n", 
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
        case DRAW:       printf("Draw\n"); break;
        case CONTINUE:   printf("Game in progress\n"); break;
    }
}

void printBitboard(Bitboard board) {
    printf("Bitboard representation:\n");
    for (int y = BOARD_SIZE - 1; y >= 0; y--) {
        printf("%d |", y + 1);
        for (int x = 0; x < BOARD_SIZE; x++) {
            Position pos = SET_POS(x, y);
            Bitboard bit = positionToBit(pos);
            printf(" %c |", (board & bit) ? '1' : '0');
        }
        printf("\n");
    }
    printf("    ");
    for (int x = 0; x < BOARD_SIZE; x++) {
        printf(" %c  ", 'a' + x);
    }
    printf("\n");
}
