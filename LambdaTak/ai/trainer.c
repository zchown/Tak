#include "trainer.h"

Trainer* createTrainer(QLearningAgent* agent, double epsilonDecay, double minEpsilon, int saveInterval) {
    Trainer* trainer = malloc(sizeof(Trainer));
    trainer->agent = agent;
    trainer->epsilonDecay = epsilonDecay;
    trainer->minEpsilon = minEpsilon;
    trainer->saveInterval = saveInterval;
    srand(time(NULL));
    return trainer;
}

void freeTrainer(Trainer* trainer) {
    free(trainer);
}

void train(Trainer* trainer, int total_episodes) {
    printf("Training...\n");
    double total_reward = 0.0;
    int whiteWins = 0, blackWins = 0, draws = 0;

    for (int episode = 0; episode < total_episodes; episode++) {
        printf("Episode %d\n", episode);
        int result = trainEpisode(trainer, episode);

        if (result == 0) {
            draws++;
        } else if (result == 1) {
            whiteWins++;
        } else {
            blackWins++;
        }

        // Decay exploration rate
        if (trainer->agent->epsilon > trainer->minEpsilon) {
            trainer->agent->epsilon *= trainer->epsilonDecay;
        }

        // Save model periodically
        if (episode % trainer->saveInterval == 0) {
            char filename[50];
            sprintf(filename, "tak_model_%d.weights", episode);
            saveWeights(trainer->agent, filename);
        }
    }

    printf("Training complete!\nWhite wins: %d\nBlack wins: %d\nDraws: %d\n",
            whiteWins, blackWins, draws);
}

int trainEpisode(Trainer* trainer, int episodeNum) {
    GameState* state = createGameState();
    Features stateFeatures = malloc(FEATURES_SIZE * sizeof(int));
    double* actionFeatures = malloc(ACTION_FEATURES_SIZE * sizeof(double));

    int gameResult = CONTINUE;
    Move lastMove;

    while (state->result == CONTINUE) {
        getFeatures(state, stateFeatures);

        GeneratedMoves* moves = generateAllMoves(state, 512);
        Move move = selectAction(trainer->agent, state, moves, stateFeatures);
        lastMove = move;

        GameState* nextState = copyGameState(state);
        MoveResult moveResult = makeMoveChecks(nextState, &move);

        Features nextStateFeatures = malloc(FEATURES_SIZE * sizeof(int));
        getFeatures(nextState, nextStateFeatures);

        double reward = calculateReward(nextState, state);

        nextState->result = checkGameResult(nextState);

        getActionFeatures(&move, actionFeatures);

        GeneratedMoves* nextMoves = NULL;
        if (nextState->result == CONTINUE) {
            nextMoves = generateAllMoves(nextState, 512);
        }

        updateQLearning(trainer->agent, stateFeatures, actionFeatures,
                reward, nextStateFeatures, nextMoves);

        free(nextStateFeatures);
        if (nextMoves != NULL) {
            freeGeneratedMoves(nextMoves);
        }
        freeGeneratedMoves(moves);
        freeGameState(state);

        state = nextState;
        gameResult = state->result;
    }

    // Handle terminal state rewards
    double finalReward = 0.0;
    int toReturn = 0;

    switch (gameResult) {
        case ROAD_WHITE:
        case FLAT_WHITE:
            finalReward = WIN_REWARD;
            toReturn = 1;
            break;
        case ROAD_BLACK:
        case FLAT_BLACK:
            finalReward = LOSS_REWARD;
            toReturn = -1;
            break;
        case DRAW:
            finalReward = DRAW_REWARD;
            toReturn = 0;
            break;
        default:
            break;
    }

    // Get features for final state
    getFeatures(state, stateFeatures);

    // Update with the final reward (no next state)
    updateQLearning(trainer->agent, stateFeatures, actionFeatures, 
            finalReward, NULL, NULL);

    free(stateFeatures);
    free(actionFeatures);
    freeGameState(state);

    return toReturn;
}

double calculateReward(const GameState* state, const GameState* prev_state) {
    double reward = STEP_PENALTY;

    // Control-based reward
    int white_control = __builtin_popcountll(state->whiteControlled);
    int prev_white = __builtin_popcountll(prev_state->whiteControlled);
    reward += (white_control - prev_white) * 0.01;

    int black_control = __builtin_popcountll(state->blackControlled);
    int prev_black = __builtin_popcountll(prev_state->blackControlled);
    reward -= (black_control - prev_black) * 0.01;

    return reward;
}

void saveWeights(const QLearningAgent* agent, const char* filename) {
    FILE* file = fopen(filename, "wb");
    if (file) {
        fwrite(agent->theta, sizeof(double), STATE_ACTION_FEATURES_SIZE, file);
        fclose(file);
    }
}

void loadWeights(QLearningAgent* agent, const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (file) {
        fread(agent->theta, sizeof(double), STATE_ACTION_FEATURES_SIZE, file);
        fclose(file);
    }
}

int trainAgainstAlphaBeta(Trainer* trainer, int total_episodes) {
    printf("Training against Alpha-Beta player...\n");
    int wins = 0, losses = 0, draws = 0;

    for (int episode = 0; episode < total_episodes; episode++) {
        printf("Episode %d\n", episode);

        // Alternate who goes first to learn both sides
        bool agentPlaysWhite = (episode % 2 == 0);

        int result = trainEpisodeVsAlphaBeta(trainer, agentPlaysWhite);

        if (result == 0) {
            draws++;
        } else if ((result == 1 && agentPlaysWhite) || (result == -1 && !agentPlaysWhite)) {
            wins++;
        } else {
            losses++;
        }

        if (trainer->agent->epsilon > trainer->minEpsilon) {
            trainer->agent->epsilon *= trainer->epsilonDecay;
        }

        if (episode % trainer->saveInterval == 0) {
            char filename[50];
            sprintf(filename, "tak_model_vs_ab_%d.weights", episode);
            saveWeights(trainer->agent, filename);
        }
    }

    printf("Training complete!\nWins: %d\nLosses: %d\nDraws: %d\n", wins, losses, draws);
    return wins;
}

int trainEpisodeVsAlphaBeta(Trainer* trainer, bool agentPlaysWhite) {
    GameState* state = createGameState();
    Features stateFeatures = malloc(FEATURES_SIZE * sizeof(int));
    double* actionFeatures = malloc(ACTION_FEATURES_SIZE * sizeof(double));

    while (state->result == CONTINUE) {
        bool agentTurn = (state->turn == WHITE) ? agentPlaysWhite : !agentPlaysWhite;

        if (agentTurn) {
            getFeatures(state, stateFeatures);
            GeneratedMoves* moves = generateAllMoves(state, 512);
            Move move = selectAction(trainer->agent, state, moves, stateFeatures);

            GameState* nextState = copyGameState(state);
            MoveResult result = makeMoveChecks(nextState, &move);

            double reward = calculateReward(nextState, state);

            nextState->result = checkGameResult(nextState);

            getActionFeatures(&move, actionFeatures);

            Features nextStateFeatures = NULL;
            GeneratedMoves* nextMoves = NULL;

            if (nextState->result == CONTINUE) {
                nextStateFeatures = malloc(FEATURES_SIZE * sizeof(int));
                getFeatures(nextState, nextStateFeatures);
                nextMoves = generateAllMoves(nextState, 512);
            }

            updateQLearning(trainer->agent, stateFeatures, actionFeatures,
                    reward, nextStateFeatures, nextMoves);

            // Clean up
            freeGeneratedMoves(moves);
            if (nextMoves) freeGeneratedMoves(nextMoves);
            if (nextStateFeatures) free(nextStateFeatures);

            freeGameState(state);
            state = nextState;
        } else {
            Move move = iterativeDeepeningSearch(state, 100);

            getFeatures(state, stateFeatures);

            GameState* nextState = copyGameState(state);
            makeMoveChecks(nextState, &move);

            nextState->result = checkGameResult(nextState);

            getActionFeatures(&move, actionFeatures);

            if (nextState->result == CONTINUE) {
                Features nextStateFeatures = malloc(FEATURES_SIZE * sizeof(int));
                getFeatures(nextState, nextStateFeatures);
                GeneratedMoves* nextMoves = generateAllMoves(nextState, 512);

                double obsReward = 0.1;

                // Learning from opponent moves
                updateQLearning(trainer->agent, stateFeatures, actionFeatures,
                               obsReward, nextStateFeatures, nextMoves);

                freeGeneratedMoves(nextMoves);
                free(nextStateFeatures);
            }

            freeGameState(state);
            state = nextState;
        }
    }

    int gameResult = state->result;
    double finalReward = 0.0;
    int returnValue = 0;

    switch (gameResult) {
        case ROAD_WHITE:
        case FLAT_WHITE:
            finalReward = agentPlaysWhite ? WIN_REWARD : LOSS_REWARD;
            returnValue = 1; // White wins
            break;
        case ROAD_BLACK:
        case FLAT_BLACK:
            finalReward = agentPlaysWhite ? LOSS_REWARD : WIN_REWARD;
            returnValue = -1; // Black wins
            break;
        case DRAW:
            finalReward = DRAW_REWARD;
            returnValue = 0;
            break;
        default:
            break;
    }

    // Final update for the agent's last move
    updateQLearning(trainer->agent, stateFeatures, actionFeatures, 
            finalReward, NULL, NULL);

    free(stateFeatures);
    free(actionFeatures);
    freeGameState(state);

    return returnValue;
}

double evaluateAgent(QLearningAgent* agent, int numGames) {
    int wins = 0, losses = 0, draws = 0;

    for (int i = 0; i < numGames; i++) {
        bool agentPlaysWhite = (i % 2 == 0);
        GameState* state = createGameState();

        while (state->result == CONTINUE) {
            bool agentTurn = (state->turn == WHITE) ? agentPlaysWhite : !agentPlaysWhite;

            if (agentTurn) {
                double savedEpsilon = agent->epsilon;
                agent->epsilon = 0.0;

                Features stateFeatures = malloc(FEATURES_SIZE * sizeof(int));
                getFeatures(state, stateFeatures);

                GeneratedMoves* moves = generateAllMoves(state, 512);
                Move move = selectAction(agent, state, moves, stateFeatures);

                makeMoveChecks(state, &move);

                freeGeneratedMoves(moves);
                free(stateFeatures);
                agent->epsilon = savedEpsilon;
            } else {
                Move move = iterativeDeepeningSearch(state, 250);
                makeMoveChecks(state, &move);
            }

            state->result = checkGameResult(state);
        }

        if (state->result == DRAW) {
            draws++;
        } else if ((state->result == ROAD_WHITE || state->result == FLAT_WHITE) && agentPlaysWhite) {
            wins++;
        } else if ((state->result == ROAD_BLACK || state->result == FLAT_BLACK) && !agentPlaysWhite) {
            wins++;
        } else {
            losses++;
        }

        freeGameState(state);
    }

    double winRate = (double)wins / numGames;
    printf("Evaluation: %d games, Win rate: %.2f%%, Wins: %d, Losses: %d, Draws: %d\n", 
            numGames, winRate * 100, wins, losses, draws);

    return winRate;
}
