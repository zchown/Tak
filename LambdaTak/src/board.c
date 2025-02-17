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
    newState->history = NULL;
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

Square* readSquare(const Board* board, Position pos) {
    if (!board) {
        printf("readSquare: Board is NULL\n");
        return NULL;
    }
    if (!isValidPosition(pos)) {
        printf("readSquare: Invalid position\n");
        return NULL;
    }
    u32 index = positionToIndex(pos);
    return (Square*)&board->squares[index];
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
    return (square ? (square->head == NULL) : true);
}

Move* createPlaceMove(Position pos, Color color, Stone stone) {
    Move* move = malloc(sizeof(Move));
    if (!move) {
        printf("createPlaceMove: Failed to allocate memory for move\n");
        return NULL;
    }
    move->type = PLACE;
    move->move.place.pos = pos;
    move->move.place.color = color;
    move->move.place.stone = stone;
    return move;
}

Move* createSlideMove(Color color, Position startPos, Direction direction, u8 count, u8* drops, Crush crush) {
    Move* move = malloc(sizeof(Move));
    if (!move) {
        printf("createSlideMove: Failed to allocate memory for move\n");
        return NULL;
    }
    move->type = SLIDE;
    move->move.slide.color = color;
    move->move.slide.startPos = startPos;
    move->move.slide.direction = direction;
    move->move.slide.count = count;
    move->move.slide.crush = crush;
    for (int i = 0; i < count; i++) {
        move->move.slide.drops[i] = drops[i];
    }
    return move;
}

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

Move* parseMove(const char* moveStr, Color color) {
    if (!moveStr) {
        printf("parseMove: Move string is NULL\n");
        return NULL;
    }
    Move* move = malloc(sizeof(Move));
    if (!move) {
        printf("parseMove: Failed to allocate memory for move\n");
        return NULL;
    }
    char firstChar = moveStr[0];
    if (firstChar == 'S' || firstChar == 'C' || strlen(moveStr) == 2) {
        move->type = PLACE;
        move->move.place.color = color;
        if (firstChar == 'S') {
            move->move.place.stone = STANDING;
            move->move.place.pos.x = moveStr[1] - 'a';
            move->move.place.pos.y = moveStr[2] - '1';
        } else if (firstChar == 'C') {
            move->move.place.stone = CAP;
            move->move.place.pos.x = moveStr[1] - 'a';
            move->move.place.pos.y = moveStr[2] - '1';
        } else {
            move->move.place.stone = FLAT;
            move->move.place.pos.x = moveStr[0] - 'a';
            move->move.place.pos.y = moveStr[1] - '1';
        }
    } else {
        move->type = SLIDE;
        move->move.slide.color = color;
        u8 offset = 0;
        if (isdigit(firstChar)) {
            move->move.slide.count = firstChar - '0';
            offset = 1;
        } else {
            move->move.slide.count = 1;
        }
        move->move.slide.startPos.x = moveStr[offset] - 'a';
        move->move.slide.startPos.y = moveStr[offset + 1] - '1';
        move->move.slide.direction = (moveStr[offset + 2] == '<') ? LEFT :
                                     (moveStr[offset + 2] == '>') ? RIGHT :
                                     (moveStr[offset + 2] == '+') ? UP : DOWN;
        if (move->move.slide.count > 1) {
            int i = 3;
            while (isdigit(moveStr[offset + i])) {
                move->move.slide.drops[i - 3] = moveStr[offset + i] - '0';
                i++;
            }
            move->move.slide.crush = (moveStr[offset + i] == '*') ? CRUSH : NO_CRUSH;
        } else {
            move->move.slide.drops[0] = 1;
            for (int i = 1; i < MAX_PICKUP; i++) {
                move->move.slide.drops[i] = 0;
            }
            move->move.slide.crush = (moveStr[offset + 3] == '*') ? CRUSH : NO_CRUSH;
        }
    }
    return move;
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
        moveStr[size++] = 'a' + move->move.place.pos.x;
        moveStr[size++] = '1' + move->move.place.pos.y;
    } else {
        if (move->move.slide.count > 1)
            moveStr[size++] = '0' + move->move.slide.count;
        moveStr[size++] = 'a' + move->move.slide.startPos.x;
        moveStr[size++] = '1' + move->move.slide.startPos.y;
        switch (move->move.slide.direction) {
            case LEFT:  moveStr[size++] = '<'; break;
            case RIGHT: moveStr[size++] = '>'; break;
            case UP:    moveStr[size++] = '+'; break;
            case DOWN:  moveStr[size++] = '-'; break;
            default: break;
        }
        u8 curDrop = 0;
        while (curDrop < MAX_PICKUP && move->move.slide.drops[curDrop] != 0) {
            moveStr[size++] = '0' + move->move.slide.drops[curDrop++];
        }
        if (move->move.slide.crush == CRUSH)
            moveStr[size++] = '*';
    }
    moveStr = realloc(moveStr, size + 1);
    moveStr[size] = '\0';
    return moveStr;
}

void printMove(const Move* move) {
    if (!move) {
        printf("printMove: Move is NULL\n");
        return;
    }
    if (move->type == PLACE) {
        printf("Place: %c%d  Player: %c  Stone: %c\n",
               'a' + move->move.place.pos.x,
               move->move.place.pos.y + 1,
               (move->move.place.color == WHITE) ? '1' : '2',
               (move->move.place.stone == FLAT) ? '-' :
               (move->move.place.stone == STANDING) ? 'S' : 'C');
    } else {
        printf("Slide: %c%d  Dir: %c  Crush: %c  Count: %d",
               'a' + move->move.slide.startPos.x,
               move->move.slide.startPos.y + 1,
               (move->move.slide.direction == LEFT)  ? '<' :
               (move->move.slide.direction == RIGHT) ? '>' :
               (move->move.slide.direction == UP)    ? '+' : '-',
               (move->move.slide.crush == CRUSH) ? '*' : ' ',
               move->move.slide.count);
        for (int i = 0; i < move->move.slide.count; i++) {
            printf(" %d", move->move.slide.drops[i]);
        }
        printf("\n");
    }
}

bool checkReservesEmpty(const GameState* state) {
    return ((state->player1.stones == 0 && state->player1.caps == 0) ||
            (state->player2.stones == 0 && state->player2.caps == 0));
}

bool checkRoad(const Board* board, Color color, SearchDirection dir) {
    bool visited[TOTAL_SQUARES] = {false};
    u32 stack[TOTAL_SQUARES];
    u8 stackSize = 0;
    for (int i = 0; i < BOARD_SIZE; i++) {
        Position start = (dir == VERTICAL) ? (Position){i, 0} : (Position){0, i};
        Square* square = readSquare(board, start);
        if (square && square->head && square->head->color == color && square->head->stone != STANDING) {
            u32 index = positionToIndex(start);
            visited[index] = true;
            stack[stackSize++] = index;
        }
    }
    while (stackSize > 0) {
        u32 index = stack[--stackSize];
        Position pos = indexToPosition(index);
        if ((dir == VERTICAL && pos.y == BOARD_SIZE - 1) ||
            (dir == HORIZONTAL && pos.x == BOARD_SIZE - 1)) {
            return true;
        }
        Position neighbors[4] = {
            {pos.x - 1, pos.y}, {pos.x + 1, pos.y},
            {pos.x, pos.y - 1}, {pos.x, pos.y + 1}
        };
        for (int i = 0; i < 4; i++) {
            Position neighbor = neighbors[i];
            if (isValidPosition(neighbor)) {
                u32 neighborIndex = positionToIndex(neighbor);
                Square* neighborSquare = readSquare(board, neighbor);
                if (!visited[neighborIndex] && neighborSquare && neighborSquare->head &&
                    neighborSquare->head->color == color && neighborSquare->head->stone != STANDING) {
                    visited[neighborIndex] = true;
                    stack[stackSize++] = neighborIndex;
                }
            }
        }
    }
    return false;
}

Result checkRoadWin(const GameState* state) {
    const Color* c = &state->turn;
    const Color o = oppositeColor(*c);
    const Board* board = state->board;
    if (checkRoad(board, *c, VERTICAL) || checkRoad(board, *c, HORIZONTAL)) {
        return (*c == WHITE) ? ROAD_WHITE : ROAD_BLACK;
    }
    if (checkRoad(board, o, VERTICAL) || checkRoad(board, o, HORIZONTAL)) {
        return (o == WHITE) ? ROAD_WHITE : ROAD_BLACK;
    }
    return CONTINUE;
}

Result checkFullBoard(const GameState* state, bool emptyReserves) {
    bool fullBoard = true;
    u8 numWhiteFlats = 0;
    u8 numBlackFlats = 0;
    for (int i = 0; i < TOTAL_SQUARES && (fullBoard || emptyReserves); i++) {
        Square* sq = (Square*)&state->board->squares[i];
        if (sq->numPieces == 0) {
            fullBoard = false;
        } else {
            Piece* head = sq->head;
            if (head->stone != STANDING) {
                if (head->color == WHITE)
                    numWhiteFlats++;
                else
                    numBlackFlats++;
            }
        }
    }
    if (!fullBoard && !emptyReserves)
        return CONTINUE;
    else if (numWhiteFlats > numBlackFlats)
        return FLAT_WHITE;
    else if (numBlackFlats > numWhiteFlats)
        return FLAT_BLACK;
    else
        return DRAW;
}

Result checkGameResult(const GameState* state) {
    Result roadResult = checkRoadWin(state);
    if (roadResult != CONTINUE) {
        return roadResult;
    }
    bool reservesEmpty = checkReservesEmpty(state);
    return checkFullBoard(state, reservesEmpty);
}

u32 positionToIndex(Position pos) {
    return pos.y * BOARD_SIZE + pos.x;
}

Position indexToPosition(u32 index) {
    Position pos = { index % BOARD_SIZE, index / BOARD_SIZE };
    return pos;
}

bool isValidPosition(Position pos) {
    return pos.x >= 0 && pos.x < BOARD_SIZE && pos.y >= 0 && pos.y < BOARD_SIZE;
}

Color oppositeColor(Color color) {
    return (color == WHITE) ? BLACK : WHITE;
}

Position nextPosition(Position pos, Direction dir) {
    Position newPos = pos;
    switch (dir) {
        case LEFT:
            if (pos.x > 0) newPos.x--;
            break;
        case RIGHT:
            if (pos.x < BOARD_SIZE - 1) newPos.x++;
            break;
        case UP:
            if (pos.y < BOARD_SIZE - 1) newPos.y++;
            break;
        case DOWN:
            if (pos.y > 0) newPos.y--;
            break;
    }
    return newPos;
}

Position slidePosition(Position pos, Direction dir, u8 count) {
    Position newPos = pos;
    for (int i = 0; i < count; i++) {
        newPos = nextPosition(newPos, dir);
    }
    return newPos;
}

Position* getNeighbors(Position pos) {
    u8 numNeighbors = 4;
    if ((pos.x == 0 || pos.x == BOARD_SIZE - 1) && (pos.y == 0 || pos.y == BOARD_SIZE - 1)) {
        numNeighbors = 2;
    } else if (pos.x == 0 || pos.x == BOARD_SIZE - 1 || pos.y == 0 || pos.y == BOARD_SIZE - 1) {
        numNeighbors = 3;
    }
    Position* neighbors = malloc(numNeighbors * sizeof(Position));
    if (!neighbors) {
        printf("getNeighbors: Failed to allocate memory for neighbors\n");
        return NULL;
    }
    u8 index = 0;
    if (pos.x > 0)
        neighbors[index++] = (Position){pos.x - 1, pos.y};
    if (pos.x < BOARD_SIZE - 1)
        neighbors[index++] = (Position){pos.x + 1, pos.y};
    if (pos.y < BOARD_SIZE - 1)
        neighbors[index++] = (Position){pos.x, pos.y + 1};
    if (pos.y > 0)
        neighbors[index++] = (Position){pos.x, pos.y - 1};
    return neighbors;
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
            Position pos = { x, y };
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

