#include "monteCarloGraphSearch.h"

Move monteCarloGraphSearch(GameState* state, DenseNeuralNet* net, bool trainingMode) {
    if (!monteCarloTable) {
        monteCarloTable = createMonteCarloTable();
    }

    MCGSStats stats = createMonteCarloStats();
    clock_t startTime = clock();

    MCGSNode* root = createMCGSNode(state->hash);
    MonteCarloTableEntry* rootEntry = lookupAndCreate(monteCarloTable, state->hash, root);

    int numIterations = 1600; 
    for (int i = 0; i < numIterations; i++) {
        GameState* stateCopy = copyGameState(state);
        SelectExpandResult result = selectExpand(monteCarloTable, stateCopy, net, root, &stats);
        if (result.trajectory.size > stats.maxDepth) {
            stats.maxDepth = result.trajectory.size;
        }
        backPropagate(&result.trajectory, result.value, &stats);
        freeGameState(stateCopy);
    }

    stats.averageValueEstimate = root->value;
    stats.totalVisits = root->numVisits;
    clock_t endTime = clock();
    stats.executionTimeMs = ((double)(endTime - startTime) / CLOCKS_PER_SEC) * 1000;
    printMCGSStats(&stats);

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
            double temp = (root->edges[i]->n * root->edges[i]->n) * root->edges[i]->q;
            if (temp < 1.0) {
                temp = 1.0;
            }
            total += temp;
        }

        if (total <= 0) { // No visits
            int selected = rand() % root->numEdges;
            printf("Move selected (uniform): %s\n", moveToString(&root->edges[selected]->move));
            return root->edges[selected]->move;
        }

        int randomIndex = rand() % (int)total;
        int i = 0;
        while (randomIndex >= 0 && i < root->numEdges) {
            int temp = (root->edges[i]->n * root->edges[i]->n) * root->edges[i]->q;
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

        printf("Move selected: %s\n", moveToString(&root->edges[i]->move));
        return root->edges[i]->move;
    }

    // Select best move based on visit count
    MCGSEdge* bestEdge = NULL;
    int maxVisits = -1;
    for (int i = 0; i < root->numEdges; i++) {
        if (root->edges[i]->n > maxVisits) {
            maxVisits = root->edges[i]->n;
            bestEdge = root->edges[i];
        }
    }

    if (bestEdge) {
        printf("Best move selected: %s\n", moveToString(&bestEdge->move));
        return bestEdge->move;
    } else {
        GeneratedMoves* moves = generateAllMoves(state, 1024);
        Move move = moves->numMoves > 0 ? moves->moves[0] : (Move){0};
        freeGeneratedMoves(moves);
        printf("No valid moves, random fallback\n");
        return move;
    }
}

SelectExpandResult selectExpand(MonteCarloTable* table, GameState* state, DenseNeuralNet* net, MCGSNode* root, MCGSStats* stats) {
    SelectExpandResult result;
    // Initialize trajectory
    result.trajectory = createTrajectory(64);
    result.value = 0.0;

    MCGSNode* node = root;
    bool needsFree = false;

    // Traverse the tree until we reach a leaf node
    while (node->isExpanded && !node->isTerminal) {
        // Select best edge using the selection policy
        MCGSEdge* edge = selectBestEdge(node);
        if (!edge) break;

        // Append (node, edge) to trajectory
        addToTrajectory(&result.trajectory, node, edge);

        // Check if the next node is a transposition
        if (edge->target->isTransposition) {
            stats->transpositionsFound++;
            double qDelta = edge->q - edge->target->value;

            // Check if Q-delta exceeds threshold Q_epsilon
            if (qDelta > Q_EPSILON) {
                // Calculate new Q-value using the formula from the pseudocode
                double nFactor = node->numVisits;
                double newQValue = nFactor * qDelta + edge->target->value;

                // Cap Q-value between V_MIN and V_MAX
                newQValue = fmax(V_MIN, fmin(newQValue, V_MAX));

                result.value = newQValue;
                return result;
            }
        }

        // Check if the next node is terminal
        if (edge->target->isTerminal) {
            result.value = edge->target->value;
            return result;
        }

        node = edge->target;

        /* printf("making move %s\n", moveToString(&edge->move)); */
        makeMoveNoChecks(state, &edge->move, false);
        /* makeMoveChecks(state, &edge->move); */
    }

    // Expand the node
    if (!node->isExpanded && !node->isTerminal) {
        // Mark as expanded
        node->isExpanded = true;

        // Generate all possible moves
        GeneratedMoves* moves = generateAllMoves(state, 1024);

        // Create edges for all possible actions
        node->numEdges = moves->numMoves;
        stats->totalEdges += moves->numMoves;
        node->edges = calloc(moves->numMoves, sizeof(MCGSEdge*));

        for (int i = 0; i < moves->numMoves; i++) {
            // Create new edge
            node->edges[i] = calloc(1, sizeof(MCGSEdge));
            node->edges[i]->move = moves->moves[i];
            node->edges[i]->q = 0.0;
            node->edges[i]->n = 0;

            // Make the move to get the next state
            /* printf("making move %s\n", moveToString(&moves->moves[i])); */
            makeMoveNoChecks(state, &moves->moves[i], false);
            /* makeMoveChecks(state, &moves->moves[i]); */
            ZobristKey nextKey = state->hash;

            // Check if this is a transposition (existing node)
            MonteCarloTableEntry* entry = lookupMonteCarloTable(table, nextKey);
            if (entry) {
                node->edges[i]->target = entry->node;
                node->edges[i]->target->isTransposition = true;
                stats->transpositionsFound++;
            } else {
                // Create new node
                MCGSNode* newNode = createMCGSNode(state->hash);
                stats->totalNodes++;

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
                // Add to transposition table
                MonteCarloTableEntry* newEntry = lookupAndCreate(table, nextKey, newNode);
                node->edges[i]->target = newNode;
            }

            undoMoveNoChecks(state, &moves->moves[i], false);
        }

        freeGeneratedMoves(moves);

        // Evaluate the node with neural network
        double* input = gameStateToVector(state);
        double* output = feedForwardDense(net, 7*36, input, 0.0, true);
        node->value = output[0];
        free(input);
        free(output);

        // Set result value to node value
        result.value = node->value;
    }

    // undo all moves in the trajectory to get back to original state
    for (int i = result.trajectory.size - 1; i >= 0; i--) {
        MCGSEdge* edge = result.trajectory.edges[i];
        undoMoveNoChecks(state, &edge->move, false);
    }

    return result;
}

void backPropagate(Trajectory* trajectory, double value, MCGSStats* stats) {
    double qTarget = DBL_MAX;

    // Process trajectory in reverse order
    for (int i = trajectory->size - 1; i >= 0; i--) {
        MCGSNode* node = trajectory->nodes[i];
        MCGSEdge* edge = trajectory->edges[i];

        if (qTarget != DBL_MAX) {
            double qDelta = edge->q - qTarget;

            // Calculate Q_phi
            double nFactor = node->numVisits;
            double qPhi = nFactor * qDelta + node->value;

            // Calculate Q'_phi
            double qPhiPrime = fmax(V_MIN, fmin(qPhi, V_MAX));

            // Set value to Q'_phi
            value = qPhiPrime;
        } else {
            // Negate value for alternating players
            value = -value;
        }

        // Update edge statistics
        edge->q = (edge->q * edge->n + value) / (edge->n + 1);
        edge->n++;

        // Update node statistics
        node->value = (node->value * node->numVisits + value) / (node->numVisits + 1);
        node->numVisits++;

        // Set qTarget for next iteration
        if (node->isTransposition) {
            qTarget = -node->value;
        } else {
            qTarget = DBL_MAX;
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
        trajectory->nodes = realloc(trajectory->nodes, trajectory->capacity * sizeof(MCGSNode*));
        trajectory->edges = realloc(trajectory->edges, trajectory->capacity * sizeof(MCGSEdge*));

        if (trajectory->nodes && trajectory->edges) {
            trajectory->nodes[trajectory->size] = node;
            trajectory->edges[trajectory->size] = edge;
            trajectory->size++;
        } else {
            printf("Failed to resize trajectory\n");
        }
    }
}

MCGSNode* createMCGSNode(ZobristKey hash) {
    MCGSNode* node = malloc(sizeof(MCGSNode));
    if (!node) {
        printf("Failed to allocate memory for MCGSNode\n");
        return NULL;
    }
    node->numVisits = 0;
    node->value = DBL_MIN;
    node->isExpanded = false;
    node->isTerminal = false;
    node->isTransposition = false;
    node->numEdges = 0;
    node->hash = hash;
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
    table->entries = calloc(MONTECARLO_TABLE_SIZE, sizeof(MonteCarloTableEntry));
    table->size = MONTECARLO_TABLE_SIZE;
    return table;
}

void freeMonteCarloTable(MonteCarloTable* table) {
    for (int i = 0; i < table->size; i++) {
        MonteCarloTableEntry* entry = &table->entries[i];
        if (entry->node) {
            free(entry->node);
        }
    }
    free(table->entries);
    free(table);
}

MonteCarloTableEntry* createMonteCarloTableEntry(ZobristKey hash, MCGSNode* node) {
    MonteCarloTableEntry* entry = calloc(1, sizeof(MonteCarloTableEntry));
    entry->isUsed = true;
    entry->hash = hash;
    entry->node = node;
    entry->next = NULL;
    return entry;
}

void freeMonteCarloTableEntry(MonteCarloTableEntry* entry) {
    if (entry) {
        free(entry->node);
        free(entry);
    }
}

MonteCarloTableEntry* lookupMonteCarloTable(MonteCarloTable* table, ZobristKey hash) {
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

MonteCarloTableEntry* lookupAndCreate(MonteCarloTable* table, ZobristKey hash, MCGSNode* node) {
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

void updateMonteCarloTable(MonteCarloTable* table, ZobristKey hash, MCGSNode* node) {
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

MCGSStats createMonteCarloStats(void) {
    return (MCGSStats){0};
}

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
        printf("%4d | %4s | %6d | %9.2f%% | %+.4f\n", 
               i + 1,
               moveToString(&sortedEdges[i]->move),
               sortedEdges[i]->n,
               (float)sortedEdges[i]->n * 100 / root->numVisits,
               sortedEdges[i]->q);
    }
    printf("========================\n\n");
    
    free(sortedEdges);
}

