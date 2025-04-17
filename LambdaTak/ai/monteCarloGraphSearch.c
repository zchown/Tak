#include "monteCarloGraphSearch.h"

Move monteCarloGraphSearch(GameState* state, DenseNeuralNet* net,
        bool trainingMode) {
    if (!monteCarloTable) {
        monteCarloTable = createMonteCarloTable();
    }

    MCGSStats stats = createMonteCarloStats();
    clock_t startTime = clock();

    MCGSNode* root = createMCGSNode(state->hash, NULL);
    MonteCarloTableEntry* rootEntry =
        lookupAndCreate(monteCarloTable, state->hash, root);

    int numIterations = 1 << 9;
    for (int i = 0; i < numIterations; i++) {
        GameState* stateCopy = copyGameState(state);
        SelectExpandResult result =
            selectExpand(monteCarloTable, stateCopy, net, root, &stats);
        if (result.trajectory.size > stats.maxDepth) {
            stats.maxDepth = result.trajectory.size;
        }
        backPropagate(&result.trajectory, result.value, &stats);
        freeGameState(stateCopy);
        stats.iterations++;
    }

    stats.averageValueEstimate = root->value;
    stats.totalVisits = root->numVisits;
    clock_t endTime = clock();
    stats.executionTimeMs =
        ((double)(endTime - startTime) / CLOCKS_PER_SEC) * 1000;
    /* printMCGSStats( & stats); */
    /* printTopMoves(root, 5); */

    if (trainingMode) {
        if (root->numEdges == 0) {
            // Fallback if no edges
            GeneratedMoves* moves = generateAllMoves(state, 1024);
            Move move = moves->numMoves > 0 ? moves->moves[0] : (Move){0};
            freeGeneratedMoves(moves);
            printf("No valid moves in training mode, fallback\n");
            return move;
        }

        double total = 0;
        if (root->numEdges == 0) {
            printf("No edges in root node\n");
        }
        for (int i = 0; i < root->numEdges; i++) {
            double temp =
                (root->edges[i]->n * root->edges[i]->n) * root->edges[i]->q;
            if (temp < 1.0) {
                temp = 1.0;
            }
            total += temp;
        }

        if (total <= 0) {  // No visits
            int selected = rand() % root->numEdges;
            printf("Move selected (uniform): %s\n",
                    moveToString(&root->edges[selected]->move));
            return root->edges[selected]->move;
        }

        int randomIndex = rand() % (int)total;
        int i = 0;
        while (randomIndex >= 0 && i < root->numEdges) {
            int temp =
                (root->edges[i]->n * root->edges[i]->n) * root->edges[i]->q;
            if (temp < 1.0) {
                temp = 1.0;
            }
            randomIndex -= temp;
            i++;
        }
        i = i > 0 ? i - 1 : 0;

        // Ensure i is within bounds
        if (i >= root->numEdges) {
            i = root->numEdges - 1;
        }

        /* printf("Move selected: %s\n", moveToString( & root -> edges[i] ->
         * move)); */
        return root->edges[i]->move;
    }

    // Select best move based on visit count 
    // and on win loss
    MCGSEdge* bestEdge = NULL;
    int maxVisits = -1;

    for (int i = 0; i < root->numEdges; i++) {
        if (root->edges[i]->n > maxVisits) {
            maxVisits = root->edges[i]->n;
            bestEdge = root->edges[i];
        }
    }


    if (bestEdge) {
        /* printf("Best move selected: %s\n", moveToString( &bestEdge ->
         * move)); */
        return bestEdge->move;
    } else {
        GeneratedMoves* moves = generateAllMoves(state, 1024);
        Move move = moves->numMoves > 0 ? moves->moves[0] : (Move){0};
        freeGeneratedMoves(moves);
        printf("No valid moves, random fallback\n");
        return move;
    }
}

SelectExpandResult selectExpand(MonteCarloTable* table, GameState* state,
        DenseNeuralNet* net, MCGSNode* root,
        MCGSStats* stats) {
    SelectExpandResult result;
    result.trajectory = createTrajectory(64);
    result.value = 0.0;
    MCGSNode* node = root;

    while (node->isExpanded && !node->isTerminal) {
        MCGSEdge* e = selectBestEdge(node);
        if (!e) break;
        addToTrajectory(&result.trajectory, node, e);

        if (e->target->isTerminal) {
            result.value = e->target->value;
            return result;
        }

        node = e->target;
        makeMoveNoChecks(state, &e->move, false);
    }

    if (!node->isExpanded && !node->isTerminal) {
        node->isExpanded = true;
        GeneratedMoves* moves = generateAllMoves(state, 1024);
        int moveNum = moves->numMoves;
        stats->totalEdges += moveNum;

        // Evaluate with single‑head network
        double* in = gameStateToVector(state);
        double* out = feedForwardDense(net, 7 * 36, in, 0.0, true);
        node->value = out[0];
        free(in);
        free(out);

        node->numEdges = moveNum;
        node->unknownChildren = moveNum;
        node->edges = calloc(moveNum, sizeof(MCGSEdge*));
        stats->totalNodes++;

        for (int i = 0; i < moveNum; i++) {
            node->edges[i] = calloc(1, sizeof(MCGSEdge));
            node->edges[i]->move = moves->moves[i];
            node->edges[i]->q = 1.0 / (double)moveNum;  // uniform prior
            node->edges[i]->n = 0;

            makeMoveNoChecks(state, &moves->moves[i], false);
            ZobristKey hash = state->hash;
            MonteCarloTableEntry* entry = lookupMonteCarloTable(table, hash);
            if (entry) {
                node->edges[i]->target = entry->node;
                entry->node->isTransposition = true;
                stats->transpositionsFound++;
                node->unknownChildren--;
                entry->node->endInPly = 0;
            } else {
                MCGSNode* newNode = createMCGSNode(hash, node);
                Result gameResult = checkGameResult(state);
                if (gameResult != CONTINUE) {
                    newNode->isTerminal = true;
                    stats->terminalNodesHit++;
                    switch (gameResult) {
                        case FLAT_WHITE:
                        case ROAD_WHITE:
                            newNode->value = 1.0;
                            break;
                        case FLAT_BLACK:
                        case ROAD_BLACK:
                            newNode->value = -1.0;
                            break;
                        case DRAW:
                            newNode->value = 0.0;
                            break;
                        default:
                            __builtin_unreachable();
                            break;
                    }
                }

                lookupAndCreate(table, hash, newNode);
                node->edges[i]->target = newNode;
            }
            undoMoveNoChecks(state, &moves->moves[i], false);
        }
        freeGeneratedMoves(moves);
        result.value = node->value;
    }

    for (int i = result.trajectory.size - 1; i >= 0; i--) {
        undoMoveNoChecks(state, &result.trajectory.edges[i]->move, false);
    }
    return result;
}

void backPropagate(Trajectory* traj, double value, MCGSStats* stats) {
    for (int i = traj->size - 1; i >= 0; i--) {
        MCGSNode* node = traj->nodes[i];
        MCGSEdge* edge = traj->edges[i];

        // Alternate players
        value = -value;

        // Update stats
        edge->q += (value - edge->q) / (edge->n + 1);
        edge->n++;
        node->value += (value - node->value) / (node->numVisits + 1);
        node->numVisits++;

        if (!node->isTerminal && node->state == MC_UNKNOWN &&
                node->unknownChildren == 0) {
            bool sawDraw = false;
            int bestWin = INT_MAX;
            int worst = 0;
            for (int j = 0; j < node->numEdges; j++) {
                MCGSNode* c = node->edges[j]->target;
                int d = c->endInPly + 1;
                if (c->state == MC_LOSS)
                    bestWin = fmin(bestWin, d);
                else if (c->state == MC_DRAW) {
                    sawDraw = true;
                    worst = fmax(worst, d);
                } else {
                    worst = fmax(worst, d);
                }
            }
            if (bestWin < INT_MAX) {
                node->state = MC_WIN;
                node->endInPly = bestWin;
            } else if (sawDraw) {
                node->state = MC_DRAW;
                node->endInPly = worst;
            } else {
                node->state = MC_LOSS;
                node->endInPly = worst;
            }
            if (i > 0) traj->nodes[i - 1]->unknownChildren--;
        }
    }
}

MCGSEdge* selectBestEdge(MCGSNode* node) {
    double bestScore = -DBL_MAX;
    MCGSEdge* bestEdge = NULL;

    for (int i = 0; i < node->numEdges; i++) {
        MCGSEdge* edge = node->edges[i];

        // Using PUCT formula from AlphaZero
        double exploitation = edge->q;
        double exploration = CPUCT * sqrt(node->numVisits) / (1 + edge->n);
        double score = exploitation + exploration;

        if (score > bestScore) {
            bestScore = score;
            bestEdge = edge;
        }
    }

    return bestEdge;
}

Trajectory createTrajectory(int capacity) {
    Trajectory trajectory;
    trajectory.nodes = calloc(capacity, sizeof(MCGSNode*));
    trajectory.edges = calloc(capacity, sizeof(MCGSEdge*));
    trajectory.size = 0;
    trajectory.capacity = capacity;
    return trajectory;
}

void freeTrajectory(Trajectory* trajectory) {
    if (trajectory) {
        for (int i = 0; i < trajectory->size; i++) {
            if (trajectory->nodes[i]) {
                markAllAsUnused(monteCarloTable, trajectory->nodes[i]->hash);
            }
        }
        free(trajectory->nodes);
        for (int i = 0; i < trajectory->size; i++) {
            if (trajectory->edges[i]) {
                free(trajectory->edges[i]);
            }
        }
        free(trajectory->edges);
        free(trajectory);
    }
}

void addToTrajectory(Trajectory* trajectory, MCGSNode* node, MCGSEdge* edge) {
    if (trajectory->size < trajectory->capacity) {
        trajectory->nodes[trajectory->size] = node;
        trajectory->edges[trajectory->size] = edge;
        trajectory->size++;
    } else {
        trajectory->capacity *= 2;
        trajectory->nodes = realloc(trajectory->nodes,
                trajectory->capacity * sizeof(MCGSNode*));
        trajectory->edges = realloc(trajectory->edges,
                trajectory->capacity * sizeof(MCGSEdge*));

        if (trajectory->nodes && trajectory->edges) {
            trajectory->nodes[trajectory->size] = node;
            trajectory->edges[trajectory->size] = edge;
            trajectory->size++;
        } else {
            printf("Failed to resize trajectory\n");
        }
    }
}

MCGSNode* createMCGSNode(ZobristKey hash, MCGSNode* parent) {
    MCGSNode* node = malloc(sizeof(MCGSNode));
    if (!node) {
        printf("Failed to allocate memory for MCGSNode\n");
        return NULL;
    }
    node->parent = parent;
    node->numVisits = 0;
    node->value = DBL_MIN;
    node->isExpanded = false;
    node->isTerminal = false;
    node->isTransposition = false;
    node->numEdges = 0;
    node->hash = hash;
    node->unknownChildren = 0;
    node->endInPly = 1000;
    node->state = MC_UNKNOWN;
    return node;
}

void freeMCGSNode(MCGSNode* node) {
    if (node) {
        for (int i = 0; i < node->numEdges; i++) {
            if (node->edges[i]) {
                freeMCGSNode(node->edges[i]->target);
                free(node->edges[i]);
            }
        }
        free(node->edges);
        free(node);
    }
}

MonteCarloTable* createMonteCarloTable(void) {
    MonteCarloTable* table = calloc(1, sizeof(MonteCarloTable));
    table->entries =
        calloc(MONTECARLO_TABLE_SIZE, sizeof(MonteCarloTableEntry));
    table->size = MONTECARLO_TABLE_SIZE;
    return table;
}

void freeMonteCarloTable(MonteCarloTable* table) {
    for (int i = 0; i < table->size; i++) {
        MonteCarloTableEntry* entry = &table->entries[i];
        while (entry) {
            MonteCarloTableEntry* next = entry->next;
            freeMonteCarloTableEntry(entry);
            entry = next;
        }
    }
    free(table->entries);
    free(table);
}

MonteCarloTableEntry* createMonteCarloTableEntry(ZobristKey hash,
        MCGSNode* node) {
    MonteCarloTableEntry* entry = calloc(1, sizeof(MonteCarloTableEntry));
    entry->isUsed = true;
    entry->hash = hash;
    entry->node = node;
    entry->next = NULL;
    return entry;
}

void freeMonteCarloTableEntry(MonteCarloTableEntry* entry) {
    if (entry) {
        freeMonteCarloTableEntry(entry);
        free(entry);
    }
}

MonteCarloTableEntry* lookupMonteCarloTable(MonteCarloTable* table,
        ZobristKey hash) {
    u32 index = mcZobristToIndex(hash);
    MonteCarloTableEntry* entry = &table->entries[index];

    while (entry) {
        if (entry->hash == hash && !entry->isUsed) {
            return entry;
        }
        entry = entry->next;
    }
    return NULL;
}

MonteCarloTableEntry* lookupAndCreate(MonteCarloTable* table, ZobristKey hash,
        MCGSNode* node) {
    u32 index = mcZobristToIndex(hash);
    MonteCarloTableEntry* entry = &table->entries[index];

    if (entry->hash == 0) {
        entry->hash = hash;
        entry->node = node;
        return entry;
    }

    while (entry->next) {
        if (entry->hash == hash && !entry->isUsed) {
            return entry;
        }
        entry = entry->next;
    }

    if (entry->hash == hash && !entry->isUsed) {
        return entry;
    }

    MonteCarloTableEntry* newEntry = createMonteCarloTableEntry(hash, node);
    entry->next = newEntry;
    return newEntry;
}

void updateMonteCarloTable(MonteCarloTable* table, ZobristKey hash,
        MCGSNode* node) {
    u32 index = mcZobristToIndex(hash);
    MonteCarloTableEntry* entry = &table->entries[index];

    if (entry->hash == 0) {
        entry->hash = hash;
        entry->node = node;
    } else {
        while (entry->next) {
            entry = entry->next;
        }
        entry->next = createMonteCarloTableEntry(hash, node);
    }
}

void markAllAsUnused(MonteCarloTable* table, ZobristKey hash) {
    u32 index = mcZobristToIndex(hash);
    MonteCarloTableEntry* entry = &table->entries[index];

    while (entry) {
        if (entry->hash == hash) {
            entry->isUsed = false;
        }
        entry = entry->next;
    }
}

u32 mcZobristToIndex(ZobristKey hash) {
    return (u32)(hash & (MONTECARLO_TABLE_SIZE - 1));
}

MCGSStats createMonteCarloStats(void) { return (MCGSStats){0}; }

void printMCGSStats(MCGSStats* stats) {
    printf("=== Monte Carlo Graph Search Stats ===\n");
    printf("Total Nodes: %d\n", stats->totalNodes);
    printf("Total Edges: %d\n", stats->totalEdges);
    printf("Max Depth: %d\n", stats->maxDepth);
    printf("Total Visits: %d\n", stats->totalVisits);
    printf("Terminal Nodes Hit: %d\n", stats->terminalNodesHit);
    printf("Transpositions Found: %d\n", stats->transpositionsFound);
    printf("Average Value Estimate: %.4f\n", stats->averageValueEstimate);
    printf("Execution Time (ms): %.2f\n", stats->executionTimeMs);
    printf("Iterations: %d\n", stats->iterations);
    printf("=====================================\n");
}

void printTopMoves(MCGSNode* root, int numMoves) {
    // Check if node has edges
    if (root->numEdges == 0) {
        printf("No moves available.\n");
        return;
    }

    // Create a copy of edges for sorting
    MCGSEdge** sortedEdges = malloc(root->numEdges * sizeof(MCGSEdge*));
    for (int i = 0; i < root->numEdges; i++) {
        sortedEdges[i] = root->edges[i];
    }

    // Sort edges by visit count (simple bubble sort)
    for (int i = 0; i < root->numEdges - 1; i++) {
        for (int j = 0; j < root->numEdges - i - 1; j++) {
            if (sortedEdges[j]->n < sortedEdges[j + 1]->n) {
                MCGSEdge* temp = sortedEdges[j];
                sortedEdges[j] = sortedEdges[j + 1];
                sortedEdges[j + 1] = temp;
            }
        }
    }

    // Limit number of moves to print
    int movesToPrint = numMoves < root->numEdges ? numMoves : root->numEdges;

    printf("\n=== Top %d Moves ===\n", movesToPrint);
    printf("Rank | Move | Visits | %% of Total | Q-Value \n");
    printf("-----|------|--------|-----------|--------\n");

    for (int i = 0; i < movesToPrint; i++) {
        printf("%4d | %4s | %6d | %9.2f%% | %+.4f\n", i + 1,
                moveToString(&sortedEdges[i]->move), sortedEdges[i]->n,
                (float)sortedEdges[i]->n * 100 / root->numVisits,
                sortedEdges[i]->q);
    }
    printf("========================\n\n");

    free(sortedEdges);
}
