#include "searches.h"

Move killerMoves[MAX_DEPTH][KILLER_MOVES];
int historyHeuristic[NUM_COLORS][TOTAL_SQUARES][TOTAL_SQUARES];

Move iterativeDeepeningSearch(GameState* state, int timeLimit) {
    printf("Starting search\n");

    if (state->turnNumber < 3) {
        clearKillerMoves();
        clearHistoryHeuristic();
        /* clearTranspositionTable(); */
    }


    Move bestMove;
    bool hasValidMove = false;

    SearchStatistics stats = {
        .timeLimit = timeLimit,
    };

    double startTime = getTimeMs();
    double prevTime = startTime;
    bool timeUp = false;

    for (int depth = 1; !timeUp; depth++) {

        stats.maxDepth = depth;

        Move currentBestMove = negaMaxRoot(state, depth,&timeUp, startTime, timeLimit, &stats);

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

        if (i > 12) {
            curDepth = depth - 1;
        } else if ( i > 36) {
            curDepth = depth - 2;
        } else if (i > 72) {
            curDepth = depth - 3;
        } else {
            curDepth = depth;
        }

        makeMoveNoChecks(state, &moves[i], false);
        int cur = -negaMax(state, curDepth - 1, BLACK_ROAD_WIN, WHITE_ROAD_WIN, -color, timeUp, startTime, timeLimit, count, stats);
        undoMoveNoChecks(state, &moves[i], false);

        if (cur > bestScore && !(*timeUp)) {
            bestScore = cur;
            bestMove = moves[i];
        }
    }

    freeGeneratedMoves(gm);
    printf("Best move: %s, Score: %d\n", moveToString(&bestMove), bestScore);
    return bestMove;
}

int negaMax(GameState* state, int depth, int alpha, int beta, int color, bool* timeUp, double startTime, int timeLimit, u32 prevMoves, SearchStatistics* stats) {
    if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
        *timeUp = true;
        return alpha;
    }

    u32 index = zobristToIndex(state->hash);
    TranspositionEntry* te = &transpositionTable[index];
    if (te->hash == state->hash) {
        stats->transpositionHits++;
        if (te->depth >= depth) {
            switch (te->type) {
                case EXACT:
                    stats->transpositionCutOffs++;
                    return te->score;
                case UNDER:
                    if (te->score <= alpha) {
                        stats->transpositionCutOffs++;
                        return alpha;
                    }
                    break;
                case OVER:
                    if (te->score >= beta) {
                        stats->transpositionCutOffs++;
                        return beta;
                    }
                    break;
            }
        }
    } else {
        stats->transpositionMisses++;
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

    GeneratedMoves* gm = generateAllMoves(state, prevMoves);
    sortMoves(state, gm->moves, gm->numMoves);

    stats->generatedMoves += gm->numMoves;

    Move* moves = gm->moves;
    u32 count = gm->numMoves;
    Move bestMove = {0};
    int bestScore = BLACK_ROAD_WIN;
    int curDepth = depth;

    for (u32 i = 0; i < count && !(*timeUp); i++) {
        if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
            *timeUp = true;
            break;
        }

        if (i > 12) {
            curDepth = depth - 1;
        } else if (i > 36) {
            curDepth = depth - 2;
        } else if (i > 72) {
            curDepth = depth - 3;
        } else {
            curDepth = depth;
        }

        makeMoveNoChecks(state, &moves[i], false);
        int cur = -negaMax(state, curDepth - 1, -beta, -alpha, -color, timeUp, startTime, timeLimit, count, stats);
        undoMoveNoChecks(state, &moves[i], false);

        if (cur > bestScore && !(*timeUp)) {
            bestScore = cur;
            bestMove = moves[i];
        }

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
    return (u32)(hash >> 34);
    /* return (u32)(hash % TRANSPOSITION_TABLE_SIZE); */
}

#pragma inline
void updateTranspositionTable(ZobristKey hash, int score, EstimationType type, Move move, int depth, SearchStatistics* stats) {
    u32 index = zobristToIndex(hash);
    TranspositionEntry* te = &transpositionTable[index];
    if (te->depth <= depth) {
        stats->transpositionDepthRewrites++;
        te->hash = hash;
        te->score = score;
        te->type = type;
        te->move = move;
        te->depth = depth;
    } else if (te->hash != hash) {
        if (te->hash != 0) {
            stats->transpositionCollisions++;
        }
        if (abs(te->score) > abs(score)) {
            return;
        }
        te->hash = hash;
        te->score = score;
        te->type = type;
        te->move = move;
        te->depth = depth;
    }
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

        // Favor central placements
        score += 75 - (GET_X(abs(move->move.place.pos) - BOARD_SIZE / 2) +
                         GET_Y(abs(move->move.place.pos) - BOARD_SIZE / 2));
        score += historyHeuristic[state->turn][move->move.place.pos][move->move.place.pos];
    } 

    else if (move->type == SLIDE) {
        SlideMove mv = move->move.slide;
        score += 400;  // Sliding moves are generally lower priority than placements
        
        Bitboard mvBitboard = 0;
        for (int i = 0; i < mv.count; i++) {
            mvBitboard |= 1ULL << slidePosition(mv.startPos, mv.direction, i);
        }
        if (ofInterest & mvBitboard) {
            score += 1000;
        }
        // Prefer spreading more pieces
        score += mv.count * mv.count * 10;

        Position endPos = slidePosition(mv.startPos, mv.direction, mv.count);

        // Use history heuristic
        score += historyHeuristic[state->turn][mv.startPos][endPos];
    }

    // Killer move bonus
    if (memcmp(move, &killerMoves[state->turnNumber][0], sizeof(Move)) == 0) {
        score += 2000;
    } else if (memcmp(move, &killerMoves[state->turnNumber][1], sizeof(Move)) == 0) {
        score += 1800;
    }

    if (state->turnNumber < 3) {
        score = 0 - score;
    }

    score += rand() % 10;

    return score;
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
    printf("Transposition hits: %d\n", stats->transpositionHits);
    printf("Transposition misses: %d\n", stats->transpositionMisses);
    printf("Transposition depth rewrites %d\n", stats->transpositionDepthRewrites);
    printf("Transposition collisions %d\n", stats->transpositionCollisions);
    printf("Transposition cut-offs: %d\n", stats->transpositionCutOffs);
    printf("Alpha-beta cut-offs: %d\n", stats->alphaBetaCutoffs);
    printf("Moves Generated per second: %f\n", stats->generatedMoves / (stats->timeLimit / 1000.0));
    printf("Nodes per second: %f\n", stats->totalNodes / (stats->timeLimit / 1000.0));
}
