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
    state->standingStones = 0;
    state->capstones = 0;
    state->hash = computeBoardHash(state);

    double* gv = gameStateToVector(state);
    for (int i = 0; i < 7 * 36; i++) {
        state->gameVector[i] = gv[i];
    }
    free(gv);

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

    for (int i = 0; i < (7 * 36); i++) {
        newState->gameVector[i] = state->gameVector[i];
    }

    newState->whiteControlled = state->whiteControlled;
    newState->blackControlled = state->blackControlled;
    newState->emptySquares = state->emptySquares;
    newState->standingStones = state->standingStones;
    newState->capstones = state->capstones;
    newState->hash = state->hash;

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
PieceStack combineStacks(PieceStack* stack1, PieceStack* stack2) {
    if (!stack1 || !stack2) {
        printf("combineStacks: Invalid stacks\n");
        return (PieceStack){0};
    }
    u8 total = stack1->numPieces + stack2->numPieces;
    if (total > MAX_PICKUP) {
        printf("combineStacks: Combined stack exceeds MAX_PICKUP (%d)\n", MAX_PICKUP);
        return (PieceStack){0};
    }

    PieceStack newStack = (PieceStack){0};
    memcpy(newStack.pieces, stack1->pieces, stack1->numPieces * sizeof(Piece));
    memcpy(newStack.pieces + stack1->numPieces, stack2->pieces, stack2->numPieces * sizeof(Piece));
    newStack.numPieces = total;
    return newStack;
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
    square.numPieces = 0;
    square.whiteStones = 0;
    square.blackStones = 0;
    return square;
}

Square squareCopy(const Square* square) {
    Square newSquare;
    newSquare.numPieces = square->numPieces;
    memcpy(newSquare.pieces, square->pieces, square->numPieces * sizeof(Piece));
    newSquare.whiteStones = square->whiteStones;
    newSquare.blackStones = square->blackStones;
    return newSquare;
}

#pragma inline
Piece* squareInsertPiece(GameState* state, Square* square, Piece piece) {
    if (square->numPieces >= 64) {
        printf("squareInsertPiece: Square is full\n");
        return NULL;
    }
    if (square->numPieces >= 7) {
        if (square->pieces[square->numPieces - 7].color == WHITE) {
            square->whiteStones--;
        } else {
            square->blackStones--;
        }
    }
    square->pieces[square->numPieces++] = piece;
    if (piece.color == WHITE) {
        square->whiteStones++;
    } else {
        square->blackStones++;
    }
    if (state) {
        Position pos = square - state->board->squares;
        if (pos < 0 || pos >= TOTAL_SQUARES) {
            printf("squareInsertPiece: Invalid position\n");
            return NULL;
        }
        Bitboard posBit = positionToBit(pos);
        if (piece.color == WHITE) {
            state->whiteControlled |= posBit;
            state->blackControlled &= ~posBit;
        } else {
            state->blackControlled |= posBit;
            state->whiteControlled &= ~posBit;
        }
        if (piece.stone == STANDING) {
            state->standingStones |= posBit;
        } else if (piece.stone == CAP) {
            state->capstones |= posBit;
        }
        state->emptySquares &= ~posBit;
    }
    return &SQ_HEAD(square);
}

#pragma inline
Piece* squareInsertPieces(GameState* state, Square* square, PieceStack* stack) {
    if (!square || !stack || stack->numPieces == 0) {
        return NULL;
    }

    if (square->numPieces + stack->numPieces > 64) {
        printf("squareInsertPieces: Square full (%d/%d)\n", 
                square->numPieces, 64);
        return NULL;
    }

    memcpy(&square->pieces[square->numPieces], 
            stack->pieces,
            stack->numPieces * sizeof(Piece));

    square->numPieces += stack->numPieces;
    
    if (state) {
        Position pos = square - state->board->squares;
        Bitboard posBit = positionToBit(pos);
        Piece top = square->pieces[square->numPieces - 1];

        if (top.color == WHITE) {
            state->whiteControlled |= posBit;
            state->blackControlled &= ~posBit;
        } else {
            state->blackControlled |= posBit;
            state->whiteControlled &= ~posBit;
        }

        if (top.stone == STANDING) {
            state->standingStones |= posBit;
        } else if (top.stone == CAP) {
            state->capstones |= posBit;
        } 
        state->emptySquares &= ~posBit;
    }
    
    square->whiteStones = 0;
    square->blackStones = 0;
    for (int i = 1; i < 7 && (square->numPieces - i) >= 0; i++) {
        if (square->pieces[square->numPieces - i].color == WHITE) {
            square->whiteStones++;
        } else {
            square->blackStones++;
        }
    }

    return &square->pieces[square->numPieces - 1];
}

#pragma inline
Piece* squareRemovePiece(GameState* state, Square* square) {
    if (square->numPieces == 0) {
        printf("squareRemovePiece: Square is empty\n");
        return NULL;
    }
    Piece* removed = &SQ_HEAD(square);
    square->numPieces--;
    if (removed->color == WHITE) {
        square->whiteStones--;
    } else {
        square->blackStones--;
    }
    if (square->numPieces >= 7) {
        if (square->pieces[square->numPieces - 7].color == WHITE) {
            square->whiteStones++;
        } else {
            square->blackStones++;
        }
    }
    if (state) {
        Position pos = square - state->board->squares;
        Bitboard posBit = positionToBit(pos);
        if (square->numPieces > 0) {
            Piece newTop = SQ_HEAD(square);
            if (newTop.color == WHITE) {
                state->whiteControlled |= posBit;
                state->blackControlled &= ~posBit;
            } else {
                state->blackControlled |= posBit;
                state->whiteControlled &= ~posBit;
            }
        } else {
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
PieceStack squareRemovePieces(GameState* state, Square* square, u8 numPieces) {
    PieceStack stack = {0};
    if (numPieces == 0 || numPieces > square->numPieces) {
        printf("squareRemovePieces: Invalid number of pieces\n");
        exit(1);
        return stack;
    }
    u8 startIdx = square->numPieces - numPieces;
    memcpy(stack.pieces, &square->pieces[startIdx], numPieces * sizeof(Piece));
    stack.numPieces = numPieces;
    square->numPieces -= numPieces;
    if (state && square->numPieces > 0) {
        Position pos = square - state->board->squares;
        Bitboard posBit = positionToBit(pos);
        Piece top = SQ_HEAD(square);
        if (top.color == WHITE) {
            state->whiteControlled |= posBit;
            state->blackControlled &= ~posBit;
        } else {
            state->blackControlled |= posBit;
            state->whiteControlled &= ~posBit;
        }
        state->standingStones &= ~posBit;
        state->capstones &= ~posBit;
    } else if (state) {
        Position pos = square - state->board->squares;
        Bitboard posBit = positionToBit(pos);
        state->emptySquares |= posBit;
        state->whiteControlled &= ~posBit;
        state->blackControlled &= ~posBit;
        state->standingStones &= ~posBit;
        state->capstones &= ~posBit;
    }

    square->whiteStones = 0;
    square->blackStones = 0;
    for (int i = 1; i < 7 && (square->numPieces - i) >= 0; i++) {
        if (square->pieces[square->numPieces - i].color == WHITE) {
            square->whiteStones++;
        } else {
            square->blackStones++;
        }
    }

    return stack;
}

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
        if (move->move.slide.count > 1) {
            for (int i = 0; i < MAX_DROPS; i++) {
                u8 drop = (move->move.slide.drops >> (i * 3)) & 0x7;
                if (drop == 0) break;
                moveStr[size++] = '0' + drop;
            }
        }
        if (move->move.slide.crush == CRUSH)
            moveStr[size++] = '*';
    }
    moveStr = realloc(moveStr, size + 1);
    if (!moveStr) {
        printf("moveToString: Failed to reallocate memory for move string\n");
        return NULL;
    }
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
            Piece top = SQ_HEAD(sq);
            if (top.color == WHITE) {
                state->whiteControlled |= posBit;
            } else {
                state->blackControlled |= posBit;
            }
            if (top.stone == STANDING) {
                state->standingStones |= posBit;
            } else if (top.stone == CAP) {
                state->capstones |= posBit;
            }
        }
    }
}

#pragma inline
bool checkReservesEmpty(const GameState* state) {
    return ((state->player1.stones == 0 && state->player1.caps == 0) ||
            (state->player2.stones == 0 && state->player2.caps == 0));
}

static inline bool hasRoad(Bitboard playerControlled, SearchDirection dir) {
    Bitboard startMask = (dir == VERTICAL) ? ROW6 : COLA;
    Bitboard endMask = (dir == VERTICAL) ? ROW1 : COLF;
    Bitboard reachable = playerControlled & startMask;

    if (dir == VERTICAL) {
        if (!(playerControlled & ROW6) || !(playerControlled & ROW5) 
                || !(playerControlled & ROW4) || !(playerControlled & ROW3)
                || !(playerControlled & ROW2) || !(playerControlled & ROW1)) {
            return false;
        }
    }
    else {
        if (!(playerControlled & COLA) || !(playerControlled & COLB) 
                || !(playerControlled & COLC) || !(playerControlled & COLD)
                || !(playerControlled & COLE) || !(playerControlled & COLF)) {
            return false;
        }
    }

    Bitboard previous;
    do {
        previous = reachable;
        Bitboard shiftedLeft = ((reachable & ~COLF) << 1) & playerControlled;
        Bitboard shiftedRight = ((reachable & ~COLA) >> 1) & playerControlled;
        Bitboard shiftedUp = ((reachable & ~ROW6) << BOARD_SIZE) & playerControlled;
        Bitboard shiftedDown = ((reachable & ~ROW1) >> BOARD_SIZE) & playerControlled;
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
    if (hasRoad(currentControlled, VERTICAL) || hasRoad(currentControlled, HORIZONTAL)) {
        return (current == WHITE) ? ROAD_WHITE : ROAD_BLACK;
    }

    // Check opponent's roads
    if (hasRoad(opponentControlled, VERTICAL) || hasRoad(opponentControlled, HORIZONTAL)) {
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

    double diff = __builtin_popcountll(whiteFlatstones) - __builtin_popcountll(blackFlatstones);
    diff -= KOMI;
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
        for (int j = 0; j < sq->numPieces; j++) {
            Piece* p = &sq->pieces[j];
            if (p->color == WHITE) {
                if (p->stone == FLAT) numWhiteStones--;
                else if (p->stone == CAP) numWhiteCaps--;
            } else {
                if (p->stone == FLAT) numBlackStones--;
                else if (p->stone == CAP) numBlackCaps--;
            }
        }
    }
    state->player1.stones = numWhiteStones;
    state->player1.caps = numWhiteCaps;
    state->player2.stones = numBlackStones;
    state->player2.caps = numBlackCaps;
}

#pragma inline
bool movesEqual(const Move* a, const Move* b) {
    if (a->type != b->type) {
        return false;
    }
    if (a->type == PLACE) {
        return a->move.place.pos == b->move.place.pos &&
            a->move.place.color == b->move.place.color &&
            a->move.place.stone == b->move.place.stone;
    } else {
        return a->move.slide.startPos == b->move.slide.startPos &&
            a->move.slide.direction == b->move.slide.direction &&
            a->move.slide.count == b->move.slide.count &&
            a->move.slide.crush == b->move.slide.crush &&
            a->move.slide.color == b->move.slide.color &&
            a->move.slide.drops == b->move.slide.drops;
    }
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
    for (int i = 0; i < square->numPieces; i++) {
        printPiece(&square->pieces[i]);
    }
    if (square->numPieces == 0) {
        printf("__");
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

// 0-1 normalized
// -1-1 was giving me issues previously
double* gameStateToVector(const GameState* state) {
    // top 7 pieces for each square
    double* vector = (double*)malloc(TOTAL_SQUARES * (BOARD_SIZE + 1) * sizeof(double));
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        Square sq = state->board->squares[i];
        int curIndex = sq.numPieces - 1;
        for (int j = 0; j < (BOARD_SIZE + 1); j++) {
            if (curIndex >= 0) {
                if (sq.pieces[curIndex].stone == FLAT) {
                    vector[i * (BOARD_SIZE + 1) + j] = 0.6;
                } else if (sq.pieces[curIndex].stone == STANDING) {
                    vector[i * (BOARD_SIZE + 1) + j] = 0.3;
                } else {
                    vector[i * (BOARD_SIZE + 1) + j] = 1.0;
                }
                if (sq.pieces[curIndex].color == BLACK) {
                    vector[i * (BOARD_SIZE + 1) + j] *= -1;
                }
                curIndex--;
            } else {
                vector[i * (BOARD_SIZE + 1) + j] = 0.0;
            }
        }

    }
    return vector;
}

void updateSquareVector(GameState *state, int squareIndex) {
    if (!state || !state->board) return;
    Square *sq = &state->board->squares[squareIndex];
    int curIndex = sq->numPieces - 1;
    int baseIdx = squareIndex * (BOARD_SIZE + 1);
    for (int j = 0; j < (BOARD_SIZE + 1); j++) {
        double val = 0.0;
        if (curIndex >= 0) {
            // Determine base value from the stone type.
            if (sq->pieces[curIndex].stone == FLAT) {
                val = 0.8;
            } else if (sq->pieces[curIndex].stone == STANDING) {
                val = 0.6;
            } else { // CAP
                val = 0.0;
            }
            // Adjust value if the piece is black.
            if (sq->pieces[curIndex].color == BLACK) {
                val = 1.0 - val;
            }
            curIndex--;
        } else {
            val = 0.0;
        }
        state->gameVector[baseIdx + j] = val;
    }

    double* gv = gameStateToVector(state);
    for (int i = 0; i < 7 * 36; i++) {
        state->gameVector[i] = gv[i];
    }
    free(gv);
}
