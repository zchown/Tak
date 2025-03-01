#include "searches.h"
#include "board.h"

Move killerMoves[MAX_DEPTH][KILLER_MOVES];
int historyHeuristic[NUM_COLORS][TOTAL_SQUARES][TOTAL_SQUARES];
static int transpositionFill = 0;

Move iterativeDeepeningSearch(GameState* state, int timeLimit) {
    printf("Starting search\n");
    int conIndex = connectivityIndex(state);
    printf("Connectivity index: %d\n", conIndex);

    if (state->turnNumber < 3) {
        clearKillerMoves();
        clearHistoryHeuristic();
    }

    Move bestMove;
    bool hasValidMove = false;

    SearchStatistics stats = {
        .timeLimit = timeLimit,
        .transpositionFill = transpositionFill
    };

    double startTime = getTimeMs();
    double prevTime = startTime;
    bool timeUp = false;

    for (int depth = 1; !timeUp; depth++) {

        stats.maxDepth = depth;

        Move currentBestMove = negaMaxRoot(state, depth, &timeUp, startTime, timeLimit, &stats);

        bestMove = currentBestMove;
        hasValidMove = true;

        double elapsedTime = getTimeMs() - startTime;
        /* double nps = (*nodes) / ((elapsedTime - prevTime)/ 1000.0); */
        if (timeLimit > 0 && elapsedTime >= timeLimit) {
            printf("elapsedTime: %f, timeLimit: %d\n", elapsedTime, timeLimit);
            printf("Time limit reached\n");
            timeUp = true;
        }
        prevTime = elapsedTime;
    }

    printSearchStats(&stats);
    transpositionFill = stats.transpositionFill;

    return hasValidMove ? bestMove : (Move){0};
}

Move negaMaxRoot(GameState* state, int depth, bool* timeUp, double startTime, int timeLimit, SearchStatistics* stats) {
    int color = (state->turn == WHITE) ? 1 : -1;
    Move bestMove = {0};
    int bestScore = BLACK_ROAD_WIN;

    GeneratedMoves* gm = generateAllMoves(state, 512);
    sortMoves(state, gm->moves, gm->numMoves);

    stats->generatedMoves += gm->numMoves;

    Move* moves = gm->moves;
    u32 count = gm->numMoves;

    if (count > 0) {
        bestMove = moves[0];
    }

    int curDepth = depth;

    for (u32 i = 0; i < count && !(*timeUp); i++) {
        if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
            *timeUp = true;
            break;
        }

        if (depth > 3) {
            if (i == 2) {
                curDepth = depth - 1;
            } else if (i == 32) {
                curDepth = depth - 2;
            } else if (i == 64) {
                curDepth = depth - 3;
            }
        }

        makeMoveNoChecks(state, &moves[i], false);
        int cur = -negaMax(state, curDepth - 1, bestScore, WHITE_ROAD_WIN, -color, timeUp, startTime, timeLimit, count, (depth > 3), stats);

        if (depth > 3 && i >= 4) {
            stats->reducedDepthSearches++;
        }

        if (cur > bestScore && i >= 4 && depth > 3 && !(*timeUp)) {
            stats->failHighResearches++;
            cur = -negaMax(state, depth - 1, bestScore, WHITE_ROAD_WIN, -color, timeUp, startTime, timeLimit, count, (depth > 3), stats);
        }

        if (cur > bestScore) {
            bestScore = cur;
            bestMove = moves[i];
        }
        undoMoveNoChecks(state, &moves[i], false);

        // -100 for the turnNum penalty
        if (bestScore >= WHITE_FLAT_WIN - 100) {
            // we have a winning move easiest way to stop
            // is to set timeUp flag
            *timeUp = true;
            break;
        }

    }

    freeGeneratedMoves(gm);
    printf("Best move: %s, Score: %d\n", moveToString(&bestMove), bestScore);
    return bestMove;
}

int negaMax(GameState* state, int depth, int alpha, int beta, int color, bool* timeUp, double startTime, int timeLimit, u32 prevMoves, bool doReducedDepth, SearchStatistics* stats) {
    const TranspositionEntry* te = lookupTranspositionTable(state->hash, depth, alpha, beta, stats);
    if (te) {
        switch (te->type) {
            case EXACT: return te->score;
            case UNDER: if (te->score <= alpha) return alpha; break;
            case OVER: if (te->score >= beta) return beta; break;
        }
    }

    Result result = checkGameResult(state);
    if (result != CONTINUE) {
        stats->totalNodes++;
        int score = 0;
        switch (result) {
            case ROAD_WHITE: 
                // turn number prevents bm
                score = color * (WHITE_ROAD_WIN - state->turnNumber);
                break;
            case ROAD_BLACK: 
                score = color * (BLACK_ROAD_WIN + state->turnNumber);
                break;
            case FLAT_WHITE: 
                score = color * (WHITE_FLAT_WIN - state->turnNumber);
                break;
            case FLAT_BLACK: 
                score = color * (BLACK_FLAT_WIN + state->turnNumber);
                break;
            case DRAW:
                score = DRAW_SCORE;
                break;
            default:
                return 0;
        }
        updateTranspositionTable(state->hash, score, EXACT, (Move){0}, depth, stats);
        return score;
    }

    if (depth <= 0) {
        stats->totalNodes++;
        int eval = color * evaluate(state);
        updateTranspositionTable(state->hash, eval, EXACT, (Move){0}, depth, stats);
        return eval;
    }

    if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
        *timeUp = true;
        int eval = color * evaluate(state);
        updateTranspositionTable(state->hash, eval, EXACT, (Move){0}, 0, stats);
        return eval;
    }

    GeneratedMoves* gm = generateAllMoves(state, prevMoves);
    sortMoves(state, gm->moves, gm->numMoves);

    stats->generatedMoves += gm->numMoves;

    Move* moves = gm->moves;
    u32 count = gm->numMoves;
    Move bestMove = {0};
    int bestScore = beta;
    int curDepth = depth;

    for (u32 i = 0; i < count && !(*timeUp); i++) {
        if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
            *timeUp = true;
            break;
        }

        if (doReducedDepth) {
            if (i == 4) {
                curDepth = depth - 1;
            } else if (i == 32) {
                curDepth = depth - 2;
            } else if (i == 64) {
                curDepth = depth - 3;
            }
        }

        if (i >= 4 && doReducedDepth) {
            stats->reducedDepthSearches++;
        }

        makeMoveNoChecks(state, &moves[i], false);
        int cur = -negaMax(state, curDepth - 1, -beta, -alpha, -color, timeUp, startTime, timeLimit, count, doReducedDepth, stats);

        if (i >= 4 && cur > alpha) {
            stats->failHighResearches++;
            cur = -negaMax(state, depth - 1, -beta, -alpha, -color, timeUp, startTime, timeLimit, count, doReducedDepth, stats);
        }

        if (cur > bestScore && !(*timeUp)) {
            bestScore = cur;
            bestMove = moves[i];
        }

        undoMoveNoChecks(state, &moves[i], false);

        if (cur > alpha) {
            alpha = cur;
            bestMove = moves[i];

            killerMoves[state->turnNumber % MAX_DEPTH][1] = 
                killerMoves[state->turnNumber % MAX_DEPTH][0];
            killerMoves[state->turnNumber % MAX_DEPTH][0] = moves[i];

            if (moves[i].type == SLIDE) {
                SlideMove* mv = &moves[i].move.slide;
                int x = mv->startPos % 36;
                int y = slidePosition(mv->startPos, mv->direction, mv->count) % 36;
                historyHeuristic[state->turn][x][y] += depth * depth;
            } else {
                PlaceMove* mv = &moves[i].move.place;
                int x = mv->pos % 36;
                historyHeuristic[state->turn][x][x] += depth * depth;
            }

        } else if (alpha >= beta) {
            stats->alphaBetaCutoffs++;
            updateTranspositionTable(state->hash, alpha, UNDER, bestMove, depth, stats);
            break;
        }
    }

    freeGeneratedMoves(gm);

    EstimationType type = (bestScore <= alpha) ? OVER : (bestScore >= beta) ? UNDER : EXACT;
    updateTranspositionTable(state->hash, bestScore, type, bestMove, depth, stats);

    return alpha;
}

#pragma inline
static double getTimeMs() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1.0e6;
}

#pragma inline
u32 zobristToIndex(ZobristKey hash) {
    return (u32)(hash & (TRANSPOSITION_TABLE_SIZE - 1));
}

#pragma inline
const TranspositionEntry* lookupTranspositionTable(ZobristKey hash, int depth, int alpha, int beta, SearchStatistics* stats) {
    stats->transpositionLookups++;
    u32 index = zobristToIndex(hash);

    for (int i = 0; i < 8; i++) {
        u32 probe = (index + i) & (TRANSPOSITION_TABLE_SIZE - 1);
        TranspositionEntry* te = &transpositionTable[probe];

        if (te->hash == hash) {  
            stats->transpositionHits++;
            if (te->depth >= depth) {
                stats->transpositionCutOffs++;
                return te;
            }
        } else if (te->hash == 0) {
            break;
        }
    }

    stats->transpositionMisses++;
    return NULL;
}

#pragma inline
void updateTranspositionTable(ZobristKey hash, int score, EstimationType type, Move move, int depth, SearchStatistics* stats) {
    u32 index = zobristToIndex(hash);
    stats->transpositionTableUpdates++;

    for (int i = 0; i < 8; i++) {
        u32 probe = (index + i) & (TRANSPOSITION_TABLE_SIZE - 1);
        TranspositionEntry* te = &transpositionTable[probe];

        if (te->hash == 0 || te->depth < depth) {
            if (te->hash == 0) {
                stats->transpositionFill++;
            } else if (te->depth < depth) {
                stats->transpositionDepthRewrites++;
            }

            *te = (TranspositionEntry){ 
                .hash = hash, 
                    .score = score, 
                    .type = type, 
                    .move = move, 
                    .depth = depth
            };
            return;
        }
    }

    stats->transpositionCollisions++;
}

#pragma inline
int scoreMove(const GameState* state, const Move* move, const Move* bestMove) {
    int score = 0;

    if (bestMove && movesEqual(move, bestMove)) {
        return 1000000;
    }

    Bitboard whiteControlled = state->whiteControlled;
    Bitboard blackControlled = state->blackControlled;
    Bitboard whiteInterest = (whiteControlled >> 6) | (whiteControlled << 6) | 
        (whiteControlled >> 1) | (whiteControlled << 1);
    Bitboard blackInterest = (blackControlled >> 6) | (blackControlled << 6) |
        (blackControlled >> 1) | (blackControlled << 1);
    whiteInterest = whiteInterest & state->emptySquares;
    blackInterest = blackInterest & state->emptySquares;

    Bitboard ourInterest = (state->turn == WHITE) ? whiteInterest : blackInterest;
    Bitboard theirInterest = (state->turn == WHITE) ? blackInterest : whiteInterest;
    Bitboard ofInterest = blackInterest | whiteInterest;

    if (move->type == PLACE) {
        if (move->move.place.stone == CAP) {
            score += 1000;  // Capstone placements are high priority
            if (ourInterest & move->move.place.pos) {
                score += 1000;
            }
        } else if (move->move.place.stone == FLAT) {
            score += 600;
            if (ourInterest & move->move.place.pos) {
                score += 1000;
            }
        } else {
            score += 500;
            if (theirInterest & move->move.place.pos) {
                score += 1000;
            }
        }

        int minStonesRemaining = (state->player1.stones < state->player2.stones) ? state->player1.stones : state->player2.stones;
        if (minStonesRemaining < 10) {
            score += 500 * (11 - minStonesRemaining);
        }

        // Favor central placements
        score += 75 - (GET_X(abs(move->move.place.pos) - BOARD_SIZE / 2) +
                GET_Y(abs(move->move.place.pos) - BOARD_SIZE / 2));
        score += historyHeuristic[state->turn][move->move.place.pos][move->move.place.pos];
    } 

    else if (move->type == SLIDE) {
        SlideMove mv = move->move.slide;
        score += 400;

        Bitboard mvBitboard = 0;
        for (int i = 0; i < mv.count; i++) {
            mvBitboard |= 1ULL << slidePosition(mv.startPos, mv.direction, i);
        }
        if (ofInterest & mvBitboard) {
            score += 1000;
        }

        Bitboard enemyControlled = (state->turn == WHITE) ? blackControlled : whiteControlled;
        if (enemyControlled & mvBitboard) {
            score += 1000;
        }

        score += mv.count * mv.count * 10;

        Position endPos = slidePosition(mv.startPos, mv.direction, mv.count);

        score += 2 * historyHeuristic[state->turn][mv.startPos][endPos];
    }

    if (memcmp(move, &killerMoves[state->turnNumber][0], sizeof(Move)) == 0) {
        score += 2500;
    } else if (memcmp(move, &killerMoves[state->turnNumber][1], sizeof(Move)) == 0) {
        score += 2200;
    }

    if (state->turnNumber < 3) {
        score = 0 - score;
    }

    return score;
}

static MCTSNode* createNode(GameState* state, Move move, MCTSNode* parent);
static void freeNode(MCTSNode* node);
static MCTSNode* select(MCTSNode* node, GameState* state, double uctConstant);
static MCTSNode* expand(MCTSNode* node, GameState* state);
static double simulate(GameState* state);
static void backpropagate(MCTSNode* node, double result);
static double getUCTValue(MCTSNode* node, double uctConstant, int parentVisits);
static MCTSNode* getBestChildUCT(MCTSNode* node, double uctConstant);
static MCTSNode* getBestChildVisits(MCTSNode* node);

Move monteCarloTreeSearch(GameState* state, int timeLimit) {
    printf("Starting Monte Carlo Tree Search\n");

    MCTSNode* root = createNode(state, (Move){0}, NULL);

    double startTime = getTimeMs();
    int iterations = 0;
    bool timeUp = false;

    SearchStatistics stats = {
        .timeLimit = timeLimit
    };

    while (!timeUp && iterations < MAX_MCTS_ITERATIONS) {
        if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
            timeUp = true;
            break;
        }

        GameState* stateCopy = copyGameState(state);

        MCTSNode* selectedNode = select(root, stateCopy, DEFAULT_UCT_CONSTANT);

        Result result = checkGameResult(stateCopy);
        if (result == CONTINUE && !selectedNode->fullyExpanded) {
            MCTSNode* newNode = expand(selectedNode, stateCopy);
            selectedNode = newNode;

            if (!movesEqual(&selectedNode->move, &(Move){0})) {
                makeMoveNoChecks(stateCopy, &selectedNode->move, false);
            }
        }

        double simulationResult = simulate(stateCopy);

        backpropagate(selectedNode, simulationResult);

        iterations++;

        freeGameState(stateCopy);
    }

    MCTSNode* bestChild = getBestChildVisits(root);

    Move bestMove = bestChild ? bestChild->move : (Move){0};

    printf("MCTS completed with %d iterations\n", iterations);
    printf("Best move: %s, Visits: %d, Score: %.2f\n", 
            moveToString(&bestMove), 
            bestChild ? bestChild->visits : 0, 
            bestChild ? bestChild->score / bestChild->visits : 0);

    freeNode(root);

    return bestMove;
}

static MCTSNode* createNode(GameState* state, Move move, MCTSNode* parent) {
    MCTSNode* node = (MCTSNode*)malloc(sizeof(MCTSNode));
    node->hash = state->hash;
    node->move = move;
    node->visits = 0;
    node->score = 0.0;
    node->numChildren = 0;
    node->children = NULL;
    node->parent = parent;
    node->fullyExpanded = false;

    return node;
}

static void freeNode(MCTSNode* node) {
    if (!node) return;

    for (int i = 0; i < node->numChildren; i++) {
        freeNode(node->children[i]);
    }

    free(node->children);
    free(node);
}

static MCTSNode* select(MCTSNode* node, GameState* state, double uctConstant) {
    if (!node->fullyExpanded || node->numChildren == 0) {
        return node;
    }

    MCTSNode* bestChild = getBestChildUCT(node, uctConstant);

    makeMoveNoChecks(state, &bestChild->move, false);

    MCTSNode* selected = select(bestChild, state, uctConstant);

    return selected;
}

static MCTSNode* expand(MCTSNode* node, GameState* state) {
    GeneratedMoves* gm = generateAllMoves(state, 512);
    sortMoves(state, gm->moves, gm->numMoves);

    if (gm->numMoves == 0 || checkGameResult(state) != CONTINUE) {
        node->fullyExpanded = true;
        freeGeneratedMoves(gm);
        return node;
    }

    if (node->numChildren > 0) {
        bool allMovesExpanded = true;
        for (u32 i = 0; i < gm->numMoves; i++) {
            bool moveFound = false;
            for (int j = 0; j < node->numChildren; j++) {
                if (movesEqual(&gm->moves[i], &node->children[j]->move)) {
                    moveFound = true;
                    break;
                }
            }
            if (!moveFound) {
                allMovesExpanded = false;
                break;
            }
        }

        node->fullyExpanded = allMovesExpanded;

        if (allMovesExpanded) {
            freeGeneratedMoves(gm);
            return node->children[rand() % node->numChildren];
        }
    }

    Move unexpandedMove;
    bool found = false;

    while (!found && gm->numMoves > 0) {
        u32 index = rand() % gm->numMoves;
        unexpandedMove = gm->moves[index];

        found = true;

        for (int i = 0; i < node->numChildren; i++) {
            if (movesEqual(&unexpandedMove, &node->children[i]->move)) {
                found = false;
                break;
            }
        }

        if (!found) {
            gm->moves[index] = gm->moves[gm->numMoves - 1];
            gm->numMoves--;
        }
    }

    if (!found) {
        freeGeneratedMoves(gm);
        return node;
    }

    GameState* stateCopy = copyGameState(state);

    makeMoveNoChecks(stateCopy, &unexpandedMove, false);

    MCTSNode* newChild = createNode(stateCopy, unexpandedMove, node);

    node->numChildren++;
    node->children = (MCTSNode**)realloc(node->children, node->numChildren * sizeof(MCTSNode*));
    node->children[node->numChildren - 1] = newChild;

    freeGeneratedMoves(gm);
    freeGameState(stateCopy);
    return newChild;
}

static double simulate(GameState* state) {
    int maxTurns = 100;
    int turn = 0;

    Move moveStack[maxTurns];
    int stackTop = -1;

    while (turn < maxTurns) {
        Result result = checkGameResult(state);

        if (result != CONTINUE) {
            while (stackTop >= 0) {
                undoMoveNoChecks(state, &moveStack[stackTop], false);
                stackTop--;
            }
            switch (result) {
                case ROAD_WHITE: return (state->turn == WHITE) ? 1.0 : 0.0;
                case ROAD_BLACK: return (state->turn == BLACK) ? 1.0 : 0.0;
                case FLAT_WHITE: return (state->turn == WHITE) ? 1.0 : 0.0;
                case FLAT_BLACK: return (state->turn == BLACK) ? 1.0 : 0.0;
                case DRAW: return 0.5;
                default: return 0.5;
            }
        }

        GeneratedMoves* gm = generateAllMoves(state, 512);

        if (gm->numMoves == 0) {
            while (stackTop >= 0) {
                undoMoveNoChecks(state, &moveStack[stackTop], false);
                stackTop--;
            }
            freeGeneratedMoves(gm);
            break;
        }

        int scores[gm->numMoves];
        int totalScore = 0;
        for (u32 i = 0; i < gm->numMoves; i++) {
            scores[i] = scoreMove(state, &gm->moves[i], &(Move){0});
            totalScore += scores[i];
        }
        int choice = rand() % totalScore;
        int selected = 0;
        while (choice >= 0) {
            choice -= scores[selected];
            if (choice < 0) {
                break;
            }
            selected++;
        }

        moveStack[++stackTop] = gm->moves[selected];
        makeMoveNoChecks(state, &gm->moves[selected], false);
        freeGeneratedMoves(gm);
        turn++;
    }

    while (stackTop >= 0) {
        undoMoveNoChecks(state, &moveStack[stackTop], false);
        stackTop--;
    }
    return 0.5;
}

static void backpropagate(MCTSNode* node, double result) {
    while (node != NULL) {
        node->visits++;
        node->score += result;

        result = 1.0 - result;

        node = node->parent;
    }
}

static double getUCTValue(MCTSNode* node, double uctConstant, int parentVisits) {
    if (node->visits == 0) {
        return INFINITY;
    }

    double exploitation = node->score / node->visits;
    double exploration = uctConstant * sqrt(log(parentVisits) / node->visits);

    return exploitation + exploration;
}

static MCTSNode* getBestChildUCT(MCTSNode* node, double uctConstant) {
    MCTSNode* bestChild = NULL;
    double bestValue = -INFINITY;

    for (int i = 0; i < node->numChildren; i++) {
        double uctValue = getUCTValue(node->children[i], uctConstant, node->visits);

        if (uctValue > bestValue) {
            bestValue = uctValue;
            bestChild = node->children[i];
        }
    }

    return bestChild ? bestChild : (node->numChildren > 0 ? node->children[0] : NULL);
}

static MCTSNode* getBestChildVisits(MCTSNode* node) {
    if (node->numChildren == 0) {
        return NULL;
    }

    MCTSNode* bestChild = NULL;
    int maxVisits = -1;

    for (int i = 0; i < node->numChildren; i++) {
        if (node->children[i]->visits > maxVisits) {
            maxVisits = node->children[i]->visits;
            bestChild = node->children[i];
        }
    }

    return bestChild;
}

#pragma inline
int compareMoves(const GameState* state, const Move* a, const Move* b, const Move* bestMove) {
    int s = scoreMove(state, b, bestMove) - scoreMove(state, a, bestMove);
    return s;
}

void quickSortMoves(GameState* state, Move* moves, int low, int high, Move* bestMove) {
    if (low < high) {
        Move pivot = moves[high];
        int i = low - 1;
        for (int j = low; j < high; j++) {
            if (compareMoves(state, &pivot, &moves[j], bestMove) > 0) {
                i++;
                Move temp = moves[i];
                moves[i] = moves[j];
                moves[j] = temp;
            }
        }
        Move temp = moves[i + 1];
        moves[i + 1] = moves[high];
        moves[high] = temp;
        int partitionIndex = i + 1;
        quickSortMoves(state, moves, low, partitionIndex - 1, bestMove);
        quickSortMoves(state, moves, partitionIndex + 1, high, bestMove);
    }
}

void sortMoves(GameState* state, Move* moves, int numMoves) {
    u32 index = zobristToIndex(state->hash);
    TranspositionEntry* te = &transpositionTable[index];
    Move* bestMove = &te->move;
    if (movesEqual(bestMove, &(Move){0})) {
        bestMove = NULL;
    }

    quickSortMoves(state, moves, 0, numMoves - 1, bestMove);
}

void clearKillerMoves(void) {
    memset(killerMoves, 0, sizeof(killerMoves));
}

void clearHistoryHeuristic(void) {
    memset(historyHeuristic, 0, sizeof(historyHeuristic));
}

void clearTranspositionTable(void) {
    memset(transpositionTable, 0, sizeof (TranspositionEntry) * TRANSPOSITION_TABLE_SIZE);
}

void printSearchStats(const SearchStatistics* stats) {

    printf("Max depth: %d\n", stats->maxDepth);
    printf("Total nodes: %d\n", stats->totalNodes);
    printf("Generated moves: %llu\n", stats->generatedMoves);
    printf("Transposition hits percentage: %f\n", stats->transpositionHits / (double)stats->transpositionLookups * 100);
    printf("Transposition miss percentage: %f\n", stats->transpositionMisses / (double)stats->transpositionLookups * 100);
    printf("Transposition depth rewrite percentage: %f\n", stats->transpositionDepthRewrites / (double)stats->transpositionTableUpdates * 100);
    printf("Transposition collision percentage: %f\n", stats->transpositionCollisions / (double)stats->transpositionTableUpdates * 100);
    printf("Transposition cut-offs: %d\n", stats->transpositionCutOffs);
    printf("Transposition cut-offs percentage %f\n", stats->transpositionCutOffs / (double)stats->transpositionLookups * 100);
    printf("Transposition fill percentage: %f\n", stats->transpositionFill / (double)TRANSPOSITION_TABLE_SIZE * 100);
    printf("Alpha-beta cut-offs: %d\n", stats->alphaBetaCutoffs);
    printf("Fail high researches: %d\n", stats->failHighResearches);
    printf("Fail high researches percentage: %f\n", stats->failHighResearches / (double)stats->totalNodes * 100);
    printf("Moves Generated per second: %f\n", stats->generatedMoves / (stats->timeLimit / 1000.0));
    printf("Nodes per second: %f\n", stats->totalNodes / (stats->timeLimit / 1000.0));
}
