#include "monteCarlo.h"
#include <math.h>
#include <float.h>

Move monteCarloTreeSearch(GameState* originalState, int timeLimit, DenseNeuralNet* net) {
    GameState* state = copyGameState(originalState);
    double startTime = getTimeMs();
    double endTime = startTime + timeLimit;

    // FIRST: Check for immediate opponent threats before even starting MCTS
    Move threatMove;
    if (detectImmediateThreats(state, &threatMove)) {
        // If there's an immediate threat, find a defensive move
        Move defensiveMove = findDefensiveMove(state, threatMove);
        if (defensiveMove.type != 0) {  // If we found a valid defense
            freeGameState(state);
            return defensiveMove;
        }
        // If no defense exists, proceed with regular MCTS (we might find our own win)
    }

    Color rootColor = state->turn;
    MCTSNode* root = createMCTSNode(rootColor, NULL, 1.0, (Move){0});

    expand(root, state, 1.0, net);

    Result immediateResult = checkGameResult(state);
    if (immediateResult != CONTINUE) {
        root->isSolved = true;
        root->evaluation = resultToEval(immediateResult);
    }

    int curIteration = 0;
    Move bestMove = {0};
    int checkWinEvery = 100;

    // Add progressive widening - only consider top N moves initially, then expand
    int initialMoves = root->numChildren > 10 ? 10 : root->numChildren;
    int currentConsideredMoves = initialMoves;
    int iterationsPerWidening = 500;
    
    // Sort child nodes by prior probability for progressive widening
    if (root->numChildren > initialMoves) {
        // Simple insertion sort by prior
        for (u32 i = 0; i < root->numChildren; i++) {
            for (u32 j = i + 1; j < root->numChildren; j++) {
                if (root->children[j]->prior > root->children[i]->prior) {
                    // Swap pointers
                    MCTSNode* temp = root->children[i];
                    root->children[i] = root->children[j];
                    root->children[j] = temp;
                }
            }
        }
    }

    while (curIteration < MAX_MCTS_ITERATIONS && getTimeMs() < endTime) {
        // Progressively widen search
        if (curIteration % iterationsPerWidening == 0 && 
            currentConsideredMoves < root->numChildren) {
            currentConsideredMoves += 5; // Consider 5 more moves
            if (currentConsideredMoves > root->numChildren)
                currentConsideredMoves = root->numChildren;
        }
        
        // Only consider the top moves for MCTS
        root->numChildren = currentConsideredMoves;
        
        Move moveStack[MAX_MCTS_DEPTH];
        int moveCount = 0;
        GameState* stateCopy = copyGameState(state); 

        ForwardResult forwardResult = forward(root, stateCopy, moveStack, &moveCount);

        if (forwardResult == FORWARD_KNOWN_EVAL) {
            // Walk the moveStack to get the leaf node where the terminal condition was detected.
            MCTSNode* leaf = root;
            for (int i = 0; i < moveCount; i++) {
                bool found = false;
                for (u32 j = 0; j < leaf->numChildren; j++) {
                    if (movesEqualMCTS(leaf->children[j]->move, moveStack[i])) {
                        leaf = leaf->children[j];
                        found = true;
                        break;
                    }
                }
                if (!found) break;
            }
            // Now back up using the evaluation from the terminal leaf.
            backwardKnownEval(root, moveStack, moveCount, leaf->evaluation);
        } else {
            double value = simulate(stateCopy, net);
            if (rootColor == BLACK) value = -value;
            Eval eval = { .type = EVAL_VALUE, .value = value };
            backwardKnownEval(root, moveStack, moveCount, eval);
        }

        freeGameState(stateCopy);
        curIteration++;

        // Check more frequently for winning moves as time runs low
        int timeRemaining = (int)(endTime - getTimeMs());
        if (timeRemaining < 500) { // Less than 500ms remaining
            checkWinEvery = 10;
        }

        if (curIteration % checkWinEvery == 0) {
            for (u32 i = 0; i < root->numChildren; i++) {
                nodeSolver(root->children[i]);
            }
            nodeSolver(root);
            
            // Early exit if we find a proven win
            for (u32 i = 0; i < root->numChildren; i++) {
                MCTSNode* child = root->children[i];
                if (child->isSolved && child->evaluation.type == EVAL_WIN) {
                    bestMove = child->move;
                    freeMCTSNode(root);
                    freeGameState(state);
                    return bestMove;
                }
            }
        }
    }

    // Improved move selection with sophisticated temperature schedule
    double temperature;
    int moveNumber = state->turnNumber;
    
    if (moveNumber < 10) {
        // Early game - more exploration
        temperature = 1.0;
    } else if (moveNumber < 30) {
        // Mid game - gradual shift to exploitation
        temperature = 0.5;
    } else {
        // Late game - pure exploitation
        temperature = 0.1;
    }
    
    // If time is running low, always pick most visited move
    double timeRemaining = endTime - getTimeMs();
    if (timeRemaining < 100) { // Less than 100ms remaining
        temperature = 0.01; // Almost deterministic selection
    }

    // First check for any move that guarantees a win.
    for (u32 i = 0; i < root->numChildren; i++) {
        MCTSNode* child = root->children[i];
        if (child->isSolved && child->evaluation.type == EVAL_WIN) {
            bestMove = child->move;
            freeMCTSNode(root);
            freeGameState(state);
            return bestMove;
        }
    }

    // Calculate move probabilities based on visit counts and temperature
    double totalVisits = 0;
    double visitCounts[root->numChildren];
    for (u32 i = 0; i < root->numChildren; i++) {
        MCTSNode* child = root->children[i];
        visitCounts[i] = pow(child->numVisits, 1.0 / temperature);
        totalVisits += visitCounts[i];
    }
    
    // Normalize to get probabilities
    double probs[root->numChildren];
    for (u32 i = 0; i < root->numChildren; i++) {
        probs[i] = visitCounts[i] / totalVisits;
    }
    
    // Select move according to visit probabilities
    double r = (double)rand() / RAND_MAX;
    double cumProb = 0.0;
    for (u32 i = 0; i < root->numChildren; i++) {
        cumProb += probs[i];
        if (r <= cumProb) {
            bestMove = root->children[i]->move;
            break;
        }
    }
    
    // Fallback if something went wrong with probability selection
    if (bestMove.type == 0) {
        // Find most visited move as fallback
        int maxVisits = -1;
        for (u32 i = 0; i < root->numChildren; i++) {
            MCTSNode* child = root->children[i];
            if (child->numVisits > maxVisits) {
                maxVisits = child->numVisits;
                bestMove = child->move;
            }
        }
    }

    freeMCTSNode(root);
    freeGameState(state);
    return bestMove;
}

ForwardResult forward(MCTSNode* root, GameState* state, Move* moveStack, int* moveCount) {
    MCTSNode* node = root;
    *moveCount = 0;

    while (*moveCount < MAX_MCTS_DEPTH) {
        node->numVisits++;

        Result result = checkGameResult(state);
        if (result != CONTINUE) {
            node->isSolved = true;
            node->evaluation = resultToEval(result);
            return FORWARD_KNOWN_EVAL;
        }

        if (node->isSolved) {
            return FORWARD_KNOWN_EVAL;
        }

        if (node->numChildren == 0) {
            return FORWARD_NEEDS_NETWORK;
        }

        MCTSNode* bestChild = selectBestUCB(node);
        if (!bestChild) break;

        makeMoveNoChecks(state, &bestChild->move, false);
        moveStack[(*moveCount)++] = bestChild->move;

        node = bestChild;
    }

    return FORWARD_NEEDS_NETWORK;
}

Propagated backwardKnownEval(MCTSNode* root, Move* moveStack, int moveCount, Eval eval) {
    MCTSNode* node = root;
    for (int i = 0; i < moveCount; i++) {
        for (u32 j = 0; j < node->numChildren; j++) {
            if (movesEqualMCTS(node->children[j]->move, moveStack[i])) {
                node = node->children[j];
                break;
            }
        }
    }

    return backup(node, evalToValue(eval), 0.0);
}

Propagated backup(MCTSNode* node, double value, double variance) {
    MCTSNode* cur = node;
    Propagated result = {.eval = {EVAL_VALUE, value}, .variance = variance};

    while (cur) {
        nodeSolver(cur);

        if (cur->isSolved) {
            result.eval = cur->evaluation;
            result.variance = 0.0;
        } else {
            cur->valueSum += value;
            double mean = cur->valueSum / cur->numVisits;

            cur->variance += (-cur->variance + variance) / cur->numVisits;

            result.eval.type = EVAL_VALUE;
            result.eval.value = mean * DISCOUNT_FACTOR;
            result.variance = cur->variance * DISCOUNT_FACTOR * DISCOUNT_FACTOR;
        }

        value = -value;

        cur = cur->parent;
    }

    return result;
}

void nodeSolver(MCTSNode* node) {
    if (node->isSolved || node->numChildren == 0) {
        return;
    }

    for (u32 i = 0; i < node->numChildren; i++) {
        MCTSNode* child = node->children[i];

        if (child->isSolved && child->evaluation.type == EVAL_LOSS) {
            node->isSolved = true;
            node->evaluation.type = EVAL_WIN;
            node->evaluation.value = 1.0;
            return;
        }
    }

    bool all_children_solved = true;
    bool any_draw = false;

    for (u32 i = 0; i < node->numChildren; i++) {
        MCTSNode* child = node->children[i];

        if (!child->isSolved) {
            all_children_solved = false;
            break;
        }

        if (child->evaluation.type == EVAL_DRAW) {
            any_draw = true;
        } else if (child->evaluation.type != EVAL_LOSS) {
            all_children_solved = false;
            break;
        }
    }

    if (all_children_solved) {
        node->isSolved = true;
        if (any_draw) {
            node->evaluation.type = EVAL_DRAW;
            node->evaluation.value = 0.0;
        } else {
            node->evaluation.type = EVAL_LOSS;
            node->evaluation.value = -1.0;
        }
    }
}

MCTSNode* selectBestUCB(MCTSNode* node) {
    if (node->numChildren == 0) {
        return NULL;
    }

    MCTSNode* bestChild = NULL;
    double maxScore = -INFINITY;

    for (u32 i = 0; i < node->numChildren; i++) {
        if (node->children[i]->numVisits < MIN_PLAYOUTS_PER_NODE) {
            return node->children[i];
        }
    }

    for (u32 i = 0; i < node->numChildren; i++) {
        MCTSNode* child = node->children[i];
        double score = puctScore(node, child, DEFAULT_UCT_CONSTANT);
        if (score > maxScore) {
            maxScore = score;
            bestChild = child;
        }
    }

    return bestChild;
}

double puctScore(MCTSNode* parent, MCTSNode* child, double c_puct) {
    if (child->isSolved) {
        if (child->evaluation.type == EVAL_LOSS) {
            return -1000000.0;
        } else if (child->evaluation.type == EVAL_WIN) {
            return 1000000.0;
        }
        return 1000.0;
    }

    // Q value - exploitation term
    double q_value = 0.0;
    if (child->numVisits > 0) {
        q_value = (child->valueSum / (double)child->numVisits);
        if (child->toPlay == BLACK) {
            q_value = -q_value;
        }
    }

    // U value - exploration term (AlphaZero-style)
    double u_value = c_puct * child->prior * 
        sqrt((double)parent->numVisits) / (1.0 + child->numVisits);

    // No separate variance term - the exploration term already accounts for uncertainty
    return q_value + u_value;
}

double getAdaptiveCPuct(const GameState* state) {
    // Linearly decrease exploration as game progresses
    double progress = state->turnNumber / 100.0;
    if (progress > 1.0) progress = 1.0;

    double c_init = 2.5;  // Initial exploration constant
    double c_final = 1.0; // Final exploration constant

    return c_init - progress * (c_init - c_final);
}

double ucbScore(MCTSNode* parent, MCTSNode* child) {
    return puctScore(parent, child, DEFAULT_UCT_CONSTANT);
}

MCTSNode* selectNode(MCTSNode* node, GameState* state, DenseNeuralNet* net, Move* moveStack, int* moveCount) {
    MCTSNode* cur = node;
    *moveCount = 0;
    while (cur->numChildren > 0 && *moveCount < MAX_MCTS_DEPTH) {
        MCTSNode* bestChild = selectBestUCB(cur);
        if (!bestChild)
            break;

        makeMoveNoChecks(state, &bestChild->move, false);
        moveStack[(*moveCount)++] = bestChild->move;
        cur = bestChild;
    }
    return cur;
}

MCTSNode* expand(MCTSNode* node, GameState* state, double prior, DenseNeuralNet* net) {
    GeneratedMoves* gm = generateAllMoves(state, 512);

    node->children = (MCTSNode**)malloc(sizeof(MCTSNode*) * gm->numMoves);
    node->numChildren = gm->numMoves;
    Color childColor = oppositeColor(node->toPlay);

    int moveScores[gm->numMoves];
    int moveTotal = 0;

    for (u32 i = 0; i < gm->numMoves; i++) {
        Move move = gm->moves[i];
        moveScores[i] = moveHeuristic(state, &move);
        moveTotal += moveScores[i];
        node->children[i] = createMCTSNode(childColor, node, 0.5, move);

        // Check for immediate terminal state:
        makeMoveNoChecks(state, &move, false);
        Result res = checkGameResult(state);
        if (res != CONTINUE) {
            node->children[i]->isSolved = true;
            node->children[i]->evaluation = resultToEval(res);
        }
        // Undo the move so state remains unchanged.
        undoMoveNoChecks(state, &move, false);
    }

    if (moveTotal > 0) {
        for (u32 i = 0; i < gm->numMoves; i++) {
            node->children[i]->prior = (double)moveScores[i] / (double)moveTotal;
        }
    }

    freeGeneratedMoves(gm);
    return node;
}

double simulate(GameState* state, DenseNeuralNet* net) {
    Result result = checkGameResult(state);
    double* gv = gameStateToVector(state);
    double resultValue = 0;
    switch (result) {
        case ROAD_WHITE:
        case FLAT_WHITE:
            resultValue = 100000000.0;
            break;
        case ROAD_BLACK:
        case FLAT_BLACK:
            resultValue = -100000000.0;
            break;
        default:
            resultValue = feedForwardDense(net, 7 * 36, gv, 0.0, true)[0];
            resultValue -= 0.5;
            resultValue *= 2;
            break;
    }
    free(gv);
    return resultValue;
}

Eval negateEval(Eval eval) {
    switch (eval.type) {
        case EVAL_WIN:
            return (Eval){EVAL_LOSS, 0.0};
        case EVAL_LOSS:
            return (Eval){EVAL_WIN, 1.0};
        case EVAL_DRAW:
            return eval;
        case EVAL_VALUE:
            return (Eval){EVAL_VALUE, -eval.value};
    }
    return eval;
}

Eval resultToEval(Result result) {
    switch (result) {
        case ROAD_WHITE:
        case FLAT_WHITE:
            return (Eval){EVAL_WIN, 1.0};
        case ROAD_BLACK:
        case FLAT_BLACK:
            return (Eval){EVAL_LOSS, 0.0};
        case DRAW:
            return (Eval){EVAL_DRAW, 0.5};
        default:
            return (Eval){EVAL_VALUE, 0.0};
    }
}

double evalToValue(Eval eval) {
    switch (eval.type) {
        case EVAL_WIN:
            return 1.0;
        case EVAL_LOSS:
            return -1.0;
        case EVAL_DRAW:
            return 0.0;
        case EVAL_VALUE:
            return eval.value;
    }
    return eval.value;
}

MCTSNode* createMCTSNode(Color toPlay, MCTSNode* parent, double prior, Move move) {
    MCTSNode* node = (MCTSNode*)malloc(sizeof(MCTSNode));
    node->numVisits = 0;
    node->toPlay = toPlay;
    node->prior = prior;
    node->parent = parent;
    node->children = NULL;
    node->numChildren = 0;
    node->valueSum = 0;
    node->variance = 0.0;
    node->isSolved = false;
    node->evaluation.type = EVAL_VALUE;
    node->evaluation.value = 0.0;
    node->move = move;
    return node;
}

void freeMCTSNode(MCTSNode* node) {
    if (node == NULL) {
        return;
    }
    if (node->children) {
        for (u32 i = 0; i < node->numChildren; i++) {
            freeMCTSNode(node->children[i]);
        }
        free(node->children);
    }
    free(node);
}

int getAdaptiveDepth(double timeRemaining) {
    if (timeRemaining > 5000) return MAX_MCTS_DEPTH;
    if (timeRemaining > 2000) return MAX_MCTS_DEPTH / 2;
    return MAX_MCTS_DEPTH / 4;
}

bool isTerminal(MCTSNode* node) {
    return node->isSolved || node->numChildren == 0;
}

bool needsInitialization(MCTSNode* node) {
    return node->numChildren == 0;
}

bool movesEqualMCTS(Move a, Move b) {
    if (a.type != b.type) return false;

    if (a.type == PLACE) {
        return a.move.place.pos == b.move.place.pos && 
            a.move.place.stone == b.move.place.stone;
    } else {
        return a.move.slide.startPos == b.move.slide.startPos &&
            a.move.slide.direction == b.move.slide.direction &&
            a.move.slide.count == b.move.slide.count;
    }
}


#pragma inline
static double getTimeMs() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1.0e6;
}

int moveHeuristic(const GameState* state, const Move* move) {
    int score = 0;

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
            score += 1000;
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

        score += 75 - (GET_X(abs(move->move.place.pos) - BOARD_SIZE / 2) +
                GET_Y(abs(move->move.place.pos) - BOARD_SIZE / 2));
        score += historyHeuristic[state->turn][move->move.place.pos][move->move.place.pos];
    } else if (move->type == SLIDE) {
        SlideMove mv = move->move.slide;
        score += 400;

        Bitboard mvBitboard = 0;
        for (int i = 0; i < mv.count; i++) {
            if (slidePosition(mv.startPos, mv.direction, i) <= TOTAL_SQUARES) {
                mvBitboard |= 1ULL << slidePosition(mv.startPos, mv.direction, i);
            }
        }
        if (ofInterest & mvBitboard) {
            score += 1000;
        }

        Bitboard enemyControlled = (state->turn == WHITE) ? blackControlled : whiteControlled;
        if (enemyControlled & mvBitboard) {
            score += 1000;
        }
        score += mv.count * mv.count * 10;
    }

    if (state->turnNumber < 3) {
        score = 0 - score;
    }

    return score;
}

bool detectImmediateThreats(GameState* state, Move* threatMove) {
    GameState* stateCopy = copyGameState(state);

    // Change turn to opponent
    stateCopy->turn = oppositeColor(stateCopy->turn);

    // Generate all opponent moves
    GeneratedMoves* gm = generateAllMoves(stateCopy, 512);

    bool foundThreat = false;

    // Check if any opponent move leads to an immediate win
    for (u32 i = 0; i < gm->numMoves; i++) {
        Move move = gm->moves[i];
        makeMoveNoChecks(stateCopy, &move, false);

        Result result = checkGameResult(stateCopy);
        if (result == ROAD_WHITE || result == ROAD_BLACK || 
                result == FLAT_WHITE || result == FLAT_BLACK) {
            // Found a winning move for opponent
            if (threatMove) *threatMove = move;
            foundThreat = true;
            undoMoveNoChecks(stateCopy, &move, false);
            break;
        }

        undoMoveNoChecks(stateCopy, &move, false);
    }

    freeGeneratedMoves(gm);
    freeGameState(stateCopy);
    return foundThreat;
}

Move findDefensiveMove(GameState* originalState, Move threatMove) {
    GameState* state = copyGameState(originalState);
    GeneratedMoves* gm = generateAllMoves(state, 512);

    Move bestDefense = {0};
    bool foundDefense = false;

    for (u32 i = 0; i < gm->numMoves; i++) {
        Move move = gm->moves[i];
        makeMoveNoChecks(state, &move, false);

        // Check if opponent's winning move is still winning
        state->turn = oppositeColor(state->turn);
        makeMoveNoChecks(state, &threatMove, false);

        Result result = checkGameResult(state);
        if (result == CONTINUE) {
            // This move prevents the immediate threat
            bestDefense = move;
            foundDefense = true;

            // Undo opponent's move
            undoMoveNoChecks(state, &threatMove, false);

            // Undo our move
            state->turn = oppositeColor(state->turn); 
            undoMoveNoChecks(state, &move, false);

            break;
        }

        // Undo opponent's move
        undoMoveNoChecks(state, &threatMove, false);

        // Undo our move
        state->turn = oppositeColor(state->turn);
        undoMoveNoChecks(state, &move, false);
    }

    freeGeneratedMoves(gm);
    freeGameState(state);

    return bestDefense;
}
