#include "searches.h"

static double getTimeMs() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1.0e6;
}

Move iterativeDeepeningSearch(GameState* state, u64* nodes, int timeLimit) {
    printf("Starting search\n");

    Move bestMove;
    bool hasValidMove = false;
    *nodes = 0;
    double startTime = getTimeMs();
    double prevTime = startTime;
    bool timeUp = false;

    for (u8 depth = 1; !timeUp; depth++) {
        Move currentBestMove = negaMaxRoot(state, depth, nodes, &timeUp, startTime, timeLimit);
        if (!timeUp) {
            bestMove = currentBestMove;
            hasValidMove = true;
        }
        double elapsedTime = getTimeMs() - startTime;
        double nps = (*nodes) / ((elapsedTime - prevTime)/ 1000.0);
        if (timeLimit > 0 && elapsedTime >= timeLimit) {
            printf("Time limit reached\n");
            timeUp = true;
        }
        printf("Depth %d: %llu nodes Time: %f ms (%.2f Mnps)\n", depth, *nodes, (elapsedTime - prevTime), nps / 1.0e6);
        *nodes = 0;
        prevTime = elapsedTime;
    }

    return hasValidMove ? bestMove : (Move){0};
}

Move negaMaxRoot(GameState* state, u8 depth, u64* nodes, bool* timeUp, double startTime, int timeLimit) {
    int color = (state->turn == WHITE) ? 1 : -1;
    Move bestMove = {0};
    int bestScore = BLACK_ROAD_WIN;

    GeneratedMoves* gm = generateAllMoves(state, 512);
    Move* moves = gm->moves;
    u32 count = gm->numMoves;

    if (count > 0) {
        bestMove = moves[0];
    }

    for (u32 i = 0; i < count && !(*timeUp); i++) {
        if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
            *timeUp = true;
            break;
        }

        makeMoveNoChecks(state, &moves[i], false);
        int cur = -negaMax(state, depth - 1, BLACK_ROAD_WIN, WHITE_ROAD_WIN, -color, nodes, timeUp, startTime, timeLimit, count);
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

int negaMax(GameState* state, u8 depth, int alpha, int beta, int color, u64* nodes, bool* timeUp, double startTime, int timeLimit, u32 prevMoves) {
    if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
        *timeUp = true;
        return 0;
    }

    Result result = checkGameResult(state);
    if (result != CONTINUE) {
        (*nodes)++;
        switch (result) {
            case ROAD_WHITE: return color * WHITE_ROAD_WIN + depth;
            case ROAD_BLACK: return color * BLACK_ROAD_WIN - depth;
            case FLAT_WHITE: return color * WHITE_FLAT_WIN - depth;
            case FLAT_BLACK: return color * BLACK_FLAT_WIN + depth;
            case DRAW:       return DRAW_SCORE;
            default:         break;
        }
    }

    if (depth == 0) {
        (*nodes)++;
        return color * evaluate(state);
    }

    GeneratedMoves* gm = generateAllMoves(state, prevMoves);
    Move* moves = gm->moves;
    u32 count = gm->numMoves;

    for (u32 i = 0; i < count && !(*timeUp); i++) {
        if (timeLimit > 0 && (getTimeMs() - startTime) >= timeLimit) {
            *timeUp = true;
            break;
        }

        makeMoveNoChecks(state, &moves[i], false);
        int cur = -negaMax(state, depth - 1, -beta, -alpha, -color, nodes, timeUp, startTime, timeLimit, count);
        undoMoveNoChecks(state, &moves[i], false);

        if (cur > alpha) {
            alpha = cur;
        } else if (alpha >= beta) {
            break;
        }
    }

    freeGeneratedMoves(gm);
    return alpha;
}

