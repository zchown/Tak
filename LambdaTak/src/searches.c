#include "searches.h"

Move killerMoves[MAX_DEPTH][KILLER_MOVES];
int historyHeuristic[NUM_COLORS][TOTAL_SQUARES][TOTAL_SQUARES];

Move iterativeDeepeningSearch(GameState* state, u64* nodes, int timeLimit) {
    printf("Starting search\n");
    Move bestMove;
    bool hasValidMove = false;
    *nodes = 0;
    double startTime = getTimeMs();
    double prevTime = startTime;
    bool timeUp = false;

    for (int depth = 1; !timeUp; depth++) {
        Move currentBestMove = negaMaxRoot(state, depth, nodes, &timeUp, startTime, timeLimit);

        bestMove = currentBestMove;
        hasValidMove = true;

        double elapsedTime = getTimeMs() - startTime;
        double nps = (*nodes) / ((elapsedTime - prevTime)/ 1000.0);
        if (timeLimit > 0 && elapsedTime >= timeLimit) {
            printf("elapsedTime: %f, timeLimit: %d\n", elapsedTime, timeLimit);
            printf("Time limit reached\n");
            timeUp = true;
        }
        printf("Depth %d: %llu nodes Time: %f ms (%.2f Mnps)\n", depth, *nodes, (elapsedTime - prevTime), nps / 1.0e6);
        *nodes = 0;
        prevTime = elapsedTime;
    }

    return hasValidMove ? bestMove : (Move){0};
}

Move negaMaxRoot(GameState* state, int depth, u64* nodes, bool* timeUp, double startTime, int timeLimit) {
    int color = (state->turn == WHITE) ? 1 : -1;
    Move bestMove = {0};
    int bestScore = BLACK_ROAD_WIN;

    GeneratedMoves* gm = generateAllMoves(state, 512);
    sortMoves(state, gm->moves, gm->numMoves);
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

        if (i > 3) {
            curDepth = depth - 2;
        } else if ( i > 9) {
            curDepth = depth - 3;
        } else if (i > 18) {
            curDepth = depth - 4;
        } else {
            curDepth = depth;
        }

        makeMoveNoChecks(state, &moves[i], false);
        int cur = -negaMax(state, curDepth - 1, BLACK_ROAD_WIN, WHITE_ROAD_WIN, -color, nodes, timeUp, startTime, timeLimit, count);
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

int negaMax(GameState* state, int depth, int alpha, int beta, int color, u64* nodes, bool* timeUp, double startTime, int timeLimit, u32 prevMoves) {
    if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
        *timeUp = true;
        return 0;
    }

    u32 index = zobristToIndex(state->hash);
    TranspositionEntry* te = &transpositionTable[index];
    if (te->hash == state->hash) {
        if (te->depth >= depth) {
            switch (te->type) {
                case EXACT:
                    return te->score;
                case UNDER:
                    if (te->score <= alpha) {
                        return alpha;
                    }
                    break;
                case OVER:
                    if (te->score >= beta) {
                        return beta;
                    }
                    break;
            }
        }
    } 

    Result result = checkGameResult(state);
    if (result != CONTINUE) {
        (*nodes)++;
        int score = 0;
        switch (result) {
            case ROAD_WHITE: 
                // depth is added to the score to prefer faster wins
                score = color * WHITE_ROAD_WIN + depth;
                break;
            case ROAD_BLACK: 
                score = color * BLACK_ROAD_WIN - depth;
                break;
            case FLAT_WHITE: 
                score = color * WHITE_FLAT_WIN + depth;
                break;
            case FLAT_BLACK: 
                score = color * BLACK_FLAT_WIN - depth;
                break;
            case DRAW:
                score = DRAW_SCORE;
                break;
            default:
                return 0;
        }
        updateTranspositionTable(state->hash, score, EXACT, (Move){0}, depth);
        return score;
    }

    if (depth <= 0) {
        (*nodes)++;
        int eval = color * evaluate(state);
        updateTranspositionTable(state->hash, eval, EXACT, (Move){0}, depth);
        return eval;
    }

    GeneratedMoves* gm = generateAllMoves(state, prevMoves);
    sortMoves(state, gm->moves, gm->numMoves);
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

        if (i > 6) {
            curDepth = depth - 1;
        } else if (i > 18) {
            curDepth = depth - 2;
        } else if (i > 36) {
            curDepth = depth - 3;
        } else {
            curDepth = depth;
        }

        makeMoveNoChecks(state, &moves[i], false);
        int cur = -negaMax(state, curDepth - 1, -beta, -alpha, -color, nodes, timeUp, startTime, timeLimit, count);
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
            updateTranspositionTable(state->hash, alpha, UNDER, bestMove, depth);
            break;
        }
    }

    freeGeneratedMoves(gm);

    EstimationType type = (bestScore <= alpha) ? OVER : (bestScore >= beta) ? UNDER : EXACT;
    updateTranspositionTable(state->hash, bestScore, type, bestMove, depth);

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
    return (u32)(hash % TRANSPOSITION_TABLE_SIZE);
}

#pragma inline
void updateTranspositionTable(ZobristKey hash, int score, EstimationType type, Move move, int depth) {
    u32 index = zobristToIndex(hash);
    TranspositionEntry* te = &transpositionTable[index];
    if (te->depth <= depth || te->hash != hash) {
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

    if (movesEqual(move, bestMove)) {
        return 1000000;
    }

    if (move->type == PLACE) {
        if (move->move.place.stone == CAP) {
            score += 1000;  // Capstone placements are high priority
        } else {
            score += 600;
        }

        // Favor central placements
        score += 75 - (GET_X(abs(move->move.place.pos) - BOARD_SIZE / 2) +
                         GET_Y(abs(move->move.place.pos) - BOARD_SIZE / 2));
        score += historyHeuristic[state->turn][move->move.place.pos][move->move.place.pos];
    } 

    else if (move->type == SLIDE) {
        SlideMove mv = move->move.slide;
        score += 400;  // Sliding moves are generally lower priority than placements

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
    
    quickSortMoves(state, moves, 0, numMoves - 1, bestMove);
}
