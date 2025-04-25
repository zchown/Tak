#include "monteCarloGraphSearch.h"

GraphNN* graphNN = NULL;
MonteCarloTable* monteCarloTable = NULL;
MoveList* monteCarloMoves = NULL;

Move monteCarloGraphSearch(GameState* state, DenseNeuralNet* net, bool trainingMode, int sock, double* probs) {
    /* printf("Monte Carlo Graph Search\n"); */
    if (!monteCarloTable) {
        monteCarloTable = createMonteCarloTable();
    }
    if (!monteCarloMoves) {
        monteCarloMoves = createMoveList(512);
    }

    // prevent arena from running out of memory
    // only happens with really long games
    if ((monteCarloTable->allocator.used * 1.5) >= (monteCarloTable->allocator.size)) {
        resetArena(&monteCarloTable->allocator);
    }

    if (!graphNN) {
        graphNN = loadGraphNN("~/ComputerScience/Tak/LambdaTak/neurelnet.mlpackage/Data/com.apple.CoreML/model.mlmodel", 7 * 36, 1);
        if (!graphNN) {
            fprintf(stderr, "Failed to load graph neural network\n");
            return (Move){0};
        }
    } else if (trainingMode) {
        // Reset the graphNN for training mode
        freeGraphNN(graphNN);
        graphNN = loadGraphNN("~/ComputerScience/Tak/LambdaTak/neurelnet.mlpackage/Data/com.apple.CoreML/model.mlmodel", 7 * 36, 1);
        if (!graphNN) {
            fprintf(stderr, "Failed to load graph neural network\n");
            return (Move){0};
        }
    }

    MCGSStats stats = createMonteCarloStats();
    clock_t startTime = clock();

    MonteCarloTableEntry* entry = lookupAndCreate(monteCarloTable, state->hash);
    MCGSNode* root = entry->node;


    int numIterations = 1 << 12;
    if (trainingMode) {
        numIterations = 1 << 12;
    }
    for (int i = 0; i < numIterations; i++) {
        /* printf("Iteration %d\n", i); */
        GameState* stateCopy = copyGameState(state);
        SelectExpandResult result = selectExpand(monteCarloTable, stateCopy, net, root, &stats, sock, trainingMode);
        if (result.trajectory.size > stats.maxDepth) {
            stats.maxDepth = result.trajectory.size;
        }
        backPropagate(&result.trajectory, result.value, &stats);
        freeGameState(stateCopy);

        // Free trajectory
        freeTrajectory(&result.trajectory);

        stats.iterations++;
    }

    stats.averageValueEstimate = root->value;
    stats.totalVisits = root->numVisits;
    clock_t endTime = clock();
    stats.executionTimeMs =
        ((double)(endTime - startTime) / CLOCKS_PER_SEC) * 1000;
    /* printMCGSStats(&stats); */
    /* printTopMoves(root, 10); */

    if (trainingMode) {
        if (root->numEdges == 0) {
            // Fallback if no edges
            generateAllMoves(state, monteCarloMoves);
            MoveList* moves = monteCarloMoves;
            Move move = moves->numMoves > 0 ? moves->moves[0] : (Move){0};
            printf("No valid moves in training mode, fallback\n");
            return move;
        }

        double total = 0;
        if (root->numEdges == 0) {
            printf("No edges in root node\n");
        }

        SearchProb prob = {0};

        for (int i = 0; i < root->numEdges; i++) {
            double temp = (root->edges[i]->n * root->edges[i]->n);
            if (temp < 1.0) {
                temp = 1.0;
            }
            total += temp;

            // Add to search probability
            /* printf("Adding to search probability: %s, %f\n", */
                    /* moveToString(&root->edges[i]->move), (float)root->edges[i]->n); */
            addToSearchProb(&prob, root->edges[i]->move, (float)root->edges[i]->n);
        }
        /* * moves = generateAllMoves(state, 1024); */
        /* printf("Total: %f\n", total); */
        toOutput(&prob, root->value, probs);
        /* printf("Probs: "); */
        /* freeGeneratedMoves(moves); */

        if (total <= 0) {  // No visits
            int selected = rand() % root->numEdges;
            /* printf("Move selected (uniform): %s\n", */
                    /* moveToString(&root->edges[selected]->move)); */
            return root->edges[selected]->move;
        }

        int randomIndex = rand() % (int)total;
        int i = 0;
        while (randomIndex >= 0 && i < root->numEdges) {
            int temp = (root->edges[i]->n * root->edges[i]->n);
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

    // First check for immediate winning moves
    for (int i = 0; i < root->numEdges; i++) {
        if (root->edges[i]->target->state == MC_WIN) {
            return root->edges[i]->move;
        }
    }

    // Then avoid immediate losing moves
    bool allLosing = true;
    for (int i = 0; i < root->numEdges; i++) {
        if (root->edges[i]->target->state != MC_LOSS) {
            allLosing = false;
            break;
        }
    }

    // If all moves are losing, pick the one that delays loss the most
    if (allLosing && root->numEdges > 0) {
        int maxDelay = -1;
        for (int i = 0; i < root->numEdges; i++) {
            if (root->edges[i]->target->endInPly > maxDelay) {
                maxDelay = root->edges[i]->target->endInPly;
                bestEdge = root->edges[i];
            }
        }
        if (bestEdge) {
            return bestEdge->move;
        }
    }

    // Otherwise select best move based on visits
    for (int i = 0; i < root->numEdges; i++) {
        // Skip immediate losing moves
        if (root->edges[i]->target->state == MC_LOSS && !allLosing) {
            continue;
        }

        if (root->edges[i]->n > maxVisits) {
            maxVisits = root->edges[i]->n;
            bestEdge = root->edges[i];
        }
    }

    if (bestEdge) {
        /* printf("Best move selected: %s\n", moveToString( &bestEdge -> */
         /* * move)); */ 
        return bestEdge->move;
    } else {
        /* GeneratedMoves* moves = generateAllMoves(state, 1024); */
        generateAllMoves(state, monteCarloMoves);
        MoveList* moves = monteCarloMoves;
        Move move = moves->numMoves > 0 ? moves->moves[0] : (Move){0};
        printf("No valid moves, random fallback\n");
        return move;
    }
}

SelectExpandResult selectExpand(MonteCarloTable* table, GameState* state,
        DenseNeuralNet* net, MCGSNode* root,
        MCGSStats* stats, int sock, bool trainingMode) {
    SelectExpandResult result;
    result.trajectory = createTrajectory(64);
    result.value = 0.0;
    MCGSNode* node = root;

    int i = 0;
    while (node->isExpanded && !node->isTerminal) {
        i++;
        if (i > 100) {
            /* printf("Infinite loop detected\n"); */
            break;
        }
        /* printf("Node: %p, Value: %f, Visits: %d\n", node, node->value, node->numVisits); */
        MCGSEdge* e = selectBestEdge(node, &result.trajectory);
        if (!e) break;
        addToTrajectory(&result.trajectory, node, e);

        makeMoveNoChecks(state, &e->move, false);

        // Add early terminal check
        Result gameResult = checkGameResult(state);
        if (gameResult != CONTINUE) {
            e->target->isTerminal = true;
            stats->terminalNodesHit++;

            switch (gameResult) {
                case FLAT_WHITE:
                case ROAD_WHITE:
                    e->target->value = 1.0;
                    break;
                case FLAT_BLACK:
                case ROAD_BLACK:
                    e->target->value = -1.0;
                    break;
                case DRAW:
                    e->target->value = 0.0;
                    break;
                default:
                    __builtin_unreachable();
                    break;
            }

            result.value = e->target->value;

            // Update node state
            if ((state->turn == WHITE && (gameResult == FLAT_WHITE || gameResult == ROAD_WHITE)) ||
                    (state->turn == BLACK && (gameResult == FLAT_BLACK || gameResult == ROAD_BLACK))) {
                e->target->state = MC_WIN;
                e->target->endInPly = 0;
            } else if (gameResult == DRAW) {
                e->target->state = MC_DRAW;
                e->target->endInPly = 0;
            } else {
                e->target->state = MC_LOSS;
                e->target->endInPly = 0;
            }

            // Undo move before returning
            undoMoveNoChecks(state, &e->move, false);
            return result;
        }

        if (e->target->isTerminal) {
            result.value = e->target->value;
            undoMoveNoChecks(state, &e->move, false);
            return result;
        }

        node = e->target;
    }

    if (!node->isExpanded && !node->isTerminal) {
        /* printf("Expanding node\n"); */
        node->isExpanded = true;
        generateAllMoves(state, monteCarloMoves);
        MoveList* moves = monteCarloMoves;
        int moveNum = moves->numMoves;
        stats->totalEdges += moveNum;

        // Evaluate with single‑head network
        double* in = gameStateToVector(state);
        /* double* out = feedForwardDense(net, 7 * 36 * 3, in, 0.0, true); */
        double* out = malloc(66 * sizeof(double));
        predictGraphNN(graphNN, in, out);
        node->value = out[0];
        if (trainingMode) {
            // apply noise
            for (int i = 0; i < 61; i++) {
                double noise = ((double)rand() / RAND_MAX) * 0.4;
                noise -= 0.2;
                out[i + 1] = out[i + 1] + noise;
            }
        }
        /* printf("Value: %f\n", node->value); */
        /* printf("Value: %f\n", node->value); */
        SearchProb probs = outputToSearchProb(out);
        /* printf("Value: %f\n", node->value); */
        free(in);
        free(out);

        node->numEdges = moveNum;
        node->unknownChildren = moveNum;
        /* node->edges = calloc(moveNum, sizeof(MCGSEdge*)); */
        node->edges = allocate(&monteCarloTable->allocator, moveNum * sizeof(MCGSEdge*));

        stats->totalNodes++;

        for (int i = 0; i < moveNum; i++) {
            /* node->edges[i] = calloc(1, sizeof(MCGSEdge)); */
            node->edges[i] = allocate(&monteCarloTable->allocator, sizeof(MCGSEdge));
            node->edges[i]->move = moves->moves[i];

            /* printf("Move: %s\n", moveToString(&node->edges[i]->move)); */
            node->edges[i]->q = probFromSearchProb(&probs, &moves->moves[i]);
            /* printf("Move: %s, Q: %f\n", moveToString(&node->edges[i]->move), node->edges[i]->q); */
            node->edges[i]->n = 0;

            makeMoveNoChecks(state, &moves->moves[i], false);
            ZobristKey hash = state->hash;

            // Check for immediate terminal state
            Result gameResult = checkGameResult(state);

            MonteCarloTableEntry* entry = lookupMonteCarloTable(table, hash);
            if (entry) {
                node->edges[i]->target = entry->node;
                entry->node->isTransposition = true;
                stats->transpositionsFound++;
                node->unknownChildren--;
                entry->node->endInPly = 0;
            } else {
                MonteCarloTableEntry* newEntry = lookupAndCreate(table, hash);
                MCGSNode* newNode =newEntry->node;
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

                    // Update node state
                    if ((state->turn == WHITE && (gameResult == FLAT_WHITE || gameResult == ROAD_WHITE)) ||
                            (state->turn == BLACK && (gameResult == FLAT_BLACK || gameResult == ROAD_BLACK))) {
                        newNode->state = MC_WIN;
                        newNode->endInPly = 0;
                    } else if (gameResult == DRAW) {
                        newNode->state = MC_DRAW;
                        newNode->endInPly = 0;
                    } else {
                        newNode->state = MC_LOSS; 
                        newNode->endInPly = 0;
                    }
                }

                node->edges[i]->target = newNode;
            }
            undoMoveNoChecks(state, &moves->moves[i], false);
        }
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

MCGSEdge* selectBestEdge(MCGSNode* node, Trajectory* traj) {
    double bestScore = -DBL_MAX;
    MCGSEdge* bestEdge = NULL;

    // First check for immediate winning moves
    for (int i = 0; i < node->numEdges; i++) {
        if (node->edges[i]->target->state == MC_WIN) {
            return node->edges[i];
        }
    }

    // Skip immediate losing moves unless all moves are losing
    // Also skip any moves that are already in the trajectory
    bool allLosing = true;
    bool allVisited = true;
    for (int i = 0; i < node->numEdges; i++) {
        if (node->edges[i]->target->state != MC_LOSS) {
            allLosing = false;
        }
        if (node->edges[i]->target->numVisits == 0) {
            allVisited = false;
        }
        if (allLosing && allVisited) {
            break;
        }
    }

    for (int i = 0; i < node->numEdges; i++) {
        MCGSEdge* edge = node->edges[i];

        // Skip losing moves if we have alternatives
        if (edge->target->state == MC_LOSS && !allLosing) {
            continue;
        }

        if (!allVisited) {
            for (int j = 0; j < traj->size; j++) {
                if (traj->nodes[j]->hash == edge->target->hash) {
                    continue;  // Skip already visited nodes
                }
            }
        }

        // Using PUCT formula from AlphaZero
        double exploitation = edge->q;
        double exploration = CPUCT * sqrt(node->numVisits) / (1 + edge->n);

        // Add progressive bias that decreases with visits
        double progressiveBias = 0.05 / (1.0 + edge->n);

        double score = exploitation + exploration + progressiveBias;

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
        free(trajectory->edges);
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
    MCGSNode* node = allocate(&monteCarloTable->allocator, sizeof(MCGSNode));
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
        if (node->parent) {
            freeMCGSNode(node->parent);
        }
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
    table->size = MONTECARLO_TABLE_SIZE;
    table->entries = calloc(table->size, sizeof(MonteCarloTableEntry));
    if (!table->entries) {
        printf("Failed to allocate memory for Monte Carlo table\n");
        free(table);
        return NULL;
    }
    table->allocator = createArenaAllocator(10ULL * 1024 * 1024 * 1024);
    if (!table->allocator.memory) {
        printf("Failed to allocate memory for Monte Carlo table allocator\n");
        free(table->entries);
        free(table);
        return NULL;
    }
    return table;
}

void clearMonteCarloTable(MonteCarloTable* table) {
    printf("Clearing Monte Carlo table\n");
    for (int i = 0; i < table->size; i++) {
        MonteCarloTableEntry* entry = &table->entries[i];
        entry->isUsed = false;
        entry->hash = 0;
        entry->node = NULL;
        entry->next = NULL;
    }
    resetArena(&table->allocator);
}

void freeMonteCarloTable(MonteCarloTable* table) {
    printf("Freeing Monte Carlo table\n");
    for (int i = 0; i < table->size; i++) {
        MonteCarloTableEntry* entry = &table->entries[i];
        freeMonteCarloTableEntry(entry);
    }
    free(table->entries);
    free(table);
}

MonteCarloTableEntry* createMonteCarloTableEntry(ZobristKey hash,
        MCGSNode* node) {
    MonteCarloTableEntry* entry = allocate(&monteCarloTable->allocator, sizeof(MonteCarloTableEntry));
    entry->isUsed = true;
    entry->hash = hash;
    entry->node = node;
    entry->next = NULL;
    return entry;
}

void freeMonteCarloTableEntry(MonteCarloTableEntry* entry) {
    if (entry) {
        freeMonteCarloTableEntry(entry->next);
        freeMCGSNode(entry->node);
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

MonteCarloTableEntry* lookupAndCreate(MonteCarloTable* table, ZobristKey hash) {
    u32 index = mcZobristToIndex(hash);
    MonteCarloTableEntry* entry = &table->entries[index];

    if (entry->hash == 0) {
        entry->hash = hash;
        entry->node = createMCGSNode(hash, NULL);
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

    MCGSNode* node = createMCGSNode(hash, NULL);
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
