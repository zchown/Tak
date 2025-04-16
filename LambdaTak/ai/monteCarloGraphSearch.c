#include "monteCarloGraphSearch.h"

Move monteCarloGraphSearch(GameState* state, DenseNeuralNet* net) {
    if (!monteCarloTable) {
        monteCarloTable = createMonteCarloTable();
    }

    // Create root node
    MCGSNode* root = createMCGSNode();

    // Add root to the table
    MonteCarloTableEntry* rootEntry = lookupAndCreate(monteCarloTable, state->hash, root);

    // Run the search for a fixed number of iterations
    int numIterations = 1600; 
    for (int i = 0; i < numIterations; i++) {
        // Select and expand
        /* GameState* stateCopy = copyGameState(state); */
        SelectExpandResult result = selectExpand(monteCarloTable, state, net, root);

        // Backpropagate
        backPropagate(&result.trajectory, result.value);
        /* freeGameState(stateCopy); */
    }

    // Select the best move based on visit count
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
        printf("No valid moves found, falling back to random move\n");
        // Fallback to a random move if no edges
        GeneratedMoves* moves = generateAllMoves(state, 512);
        Move move = moves->numMoves > 0 ? moves->moves[0] : (Move){0};
        freeGeneratedMoves(moves);
        return move;
    }
}

SelectExpandResult selectExpand(MonteCarloTable* table, GameState* state, DenseNeuralNet* net, MCGSNode* root) {
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
        GeneratedMoves* moves = generateAllMoves(state, 512);

        // Create edges for all possible actions
        node->numEdges = moves->numMoves;
        node->edges = malloc(moves->numMoves * sizeof(MCGSEdge*));

        for (int i = 0; i < moves->numMoves; i++) {
            // Create new edge
            node->edges[i] = malloc(sizeof(MCGSEdge));
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
            } else {
                // Create new node
                MCGSNode* newNode = createMCGSNode();

                Result gameResult = checkGameResult(state);
                if (gameResult != CONTINUE) {
                    newNode->isTerminal = true;
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

    //TODO not sure if this belongs here or at callsite
    //undo moves
    for (int i = result.trajectory.size - 1; i >= 0; i--) {
        MCGSEdge* edge = result.trajectory.edges[i];
        undoMoveNoChecks(state, &edge->move, false);
    }

    return result;
}

void backPropagate(Trajectory* trajectory, double value) {
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
    trajectory.nodes = malloc(capacity * sizeof(MCGSNode*));
    trajectory.edges = malloc(capacity * sizeof(MCGSEdge*));
    trajectory.size = 0;
    trajectory.capacity = capacity;
    return trajectory;
}

void freeTrajectory(Trajectory* trajectory) {
    if (trajectory) {
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

MCGSNode* createMCGSNode(void) {
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
    MonteCarloTable* table = malloc(sizeof(MonteCarloTable));
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
    MonteCarloTableEntry* entry = malloc(sizeof(MonteCarloTableEntry));
    entry->isUsed = false;
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

u32 mcZobristToIndex(ZobristKey hash) {
    return (u32)(hash & (MONTECARLO_TABLE_SIZE - 1));
}

