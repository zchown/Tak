#include "neuralNetTrainer.h"

Trainer* createTrainer(DenseNeuralNet* net, double learningRateUpdate, double minLearningRate, double learningRate, int saveInterval, double discountFactor) {
    Trainer* trainer = (Trainer*)malloc(sizeof(Trainer));
    trainer->net = net;
    trainer->learningRateUpdate = learningRateUpdate;
    trainer->minLearningRate = minLearningRate;
    trainer->learningRate = learningRate;
    trainer->saveInterval = saveInterval;
    trainer->discountFactor = discountFactor;
    return trainer;
}

void freeTrainer(Trainer* trainer) {
    free(trainer);
}

double calculateTimeAdjustedReward(int result, int numMoves, int maxMoves) {
    double baseReward;
    switch (result) {
        case ROAD_WHITE:
            baseReward = 1.0;
            break;
        case FLAT_WHITE:
            baseReward = 0.8;
            break;
        case ROAD_BLACK:
            baseReward = 0.0;
            break;
        case FLAT_BLACK:
            baseReward = 0.2;
            break;
        case DRAW:
            baseReward = 0.5;
            break;
        default:
            baseReward = 0.5;
            break;
    }

    double moveFactor = 1.0 - ((double)numMoves / maxMoves);
    moveFactor *= moveFactor;
    double toReturn = baseReward;

    if (toReturn < 0.0) {
        toReturn = 0.0;
    } else if (toReturn > 1.0) {
        toReturn = 1.0;
    }
    return toReturn;
}

void train(Trainer* trainer, int totalEpisodes) {
    printf("Training for %d episodes\n", totalEpisodes);
    int whiteRoads = 0;
    int whiteFlats = 0;
    int blackRoads = 0;
    int blackFlats = 0;
    int draws = 0;
    for (int i = 1; i <= totalEpisodes; i++) {
        trainer->learningRate = trainer->learningRate * trainer->learningRateUpdate;
        if (trainer->learningRate < trainer->minLearningRate) {
            trainer->learningRate = trainer->minLearningRate;
        }

        printf("Episode %d: White Roads: %d, White Flats: %d, Black Roads: %d, Black Flats: %d, Draws: %d, LR: %.6f\n", 
                i, whiteRoads, whiteFlats, blackRoads, blackFlats, draws, trainer->learningRate);

        if (monteCarloTable) {
            freeMonteCarloTable(monteCarloTable);
            monteCarloTable = NULL;
        }

        int r = trainEpisode(trainer, i);
        switch (r) {
            case 2:
                whiteRoads++;
                break;
            case 1:
                whiteFlats++;
                break;
            case -2:
                blackRoads++;
                break;
            case -1:
                blackFlats++;
                break;
            case 0:
                draws++;
                break;
            default:
                break;
        }
        if (i % trainer->saveInterval == 0) {
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_verysmall");
        }
    }
    printf("\n");
}

int trainEpisode(Trainer* trainer, int episodeNum) {
    GameState* state = createGameState();

    // Store the entire sequence of states, actions, and rewards
    double** pastStates = (double**)malloc(1000 * sizeof(double*));
    double** pastOutputs = (double**)malloc(1000 * sizeof(double*));
    double* pastValues = (double*)malloc(1000 * sizeof(double));
    int numPastStates = 0;

    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        double* outputs = feedForwardDense(trainer->net, (7 * 36), inputs, 0.0, true);

        pastStates[numPastStates] = inputs;
        pastOutputs[numPastStates] = outputs;
        pastValues[numPastStates] = outputs[0];

        GeneratedMoves* moves = generateAllMoves(state, numMoves);
        numMoves = moves->numMoves;

        int random = rand() % 10;
        Move move;
        if (random < 5) {
            move = monteCarloGraphSearch(state, trainer->net, false);
        } else {
            move = monteCarloGraphSearch(state, trainer->net, true);
        }

        makeMoveNoChecks(state, &move, false);
        freeGeneratedMoves(moves);

        if (numPastStates > 0) {
            // TD(0) update: V(s) = V(s) + α[r + γV(s') - V(s)]
            double currentValue = pastValues[numPastStates];
            double prevValue = pastValues[numPastStates - 1];
            double immediateReward = pseudoReward(state) - 0.5; // Adjust to be centered around 0

            // Calculate TD error: r + γV(s') - V(s)
            double tdError = immediateReward + (trainer->discountFactor * currentValue) - prevValue;

            double targetValue = prevValue + tdError;

            // Convert back to 0-1 range for the network
            targetValue += 0.5;
            if (targetValue < 0.0) targetValue = 0.0;
            if (targetValue > 1.0) targetValue = 1.0;

            // Backpropagate the TD error for the previous state
            backpropagateDense(trainer->net, pastStates[numPastStates - 1], 
                    pastOutputs[numPastStates - 1], &targetValue, trainer->learningRate);
        }

        numPastStates++;
    }

    int gameResult = checkGameResult(state);
    int toReturn = 0;

    switch (gameResult) {
        case ROAD_WHITE: toReturn = 2; break;
        case FLAT_WHITE: toReturn = 1; break;
        case ROAD_BLACK: toReturn = -2; break;
        case FLAT_BLACK: toReturn = -1; break;
        case DRAW: toReturn = 0; break;
        default: break;
    }

    double finalReward = calculateTimeAdjustedReward(gameResult, numPastStates, 225);

    if (numPastStates > 0) {
        backpropagateDense(trainer->net, pastStates[numPastStates - 1], 
                pastOutputs[numPastStates - 1], &finalReward, trainer->learningRate);
    }

    double decayFactor = 0.9;
    for (int i = numPastStates - 2; i >= 0; i--) {
        double decay = pow(decayFactor * trainer->discountFactor, numPastStates - 1 - i);
        double targetValue = pastValues[i] + (decay * (finalReward - pastValues[i]));

        // Ensure targetValue is in valid range [0, 1]
        if (targetValue < 0.0) targetValue = 0.0;
        if (targetValue > 1.0) targetValue = 1.0;

        backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &targetValue, 
                trainer->learningRate * decay);
    }

    for (int i = 0; i < numPastStates; i++) {
        free(pastStates[i]);
        free(pastOutputs[i]);
    }
    free(pastStates);
    free(pastOutputs);
    free(pastValues);
    freeGameState(state);

    printf("numPastStates: %d, Final reward: %.4f\n", numPastStates, finalReward);
    return toReturn;
}

double pseudoReward(const GameState* state) {
    double toReturn = (__builtin_popcount(state->whiteControlled) 
            - __builtin_popcount(state->blackControlled) - KOMI) / 36.0;
    int connectivity = connectivityIndex(state);
    if (connectivity > 0) {
        toReturn += 0.2;
    } else if (connectivity < 0) {
        toReturn -= 0.2;
    }
    toReturn += 0.5;
    if (toReturn < 0.0) {
        toReturn = 0.0;
    } else if (toReturn > 1.0) {
        toReturn = 1.0;
    }
    return toReturn;
}


void trainAlphaBeta(Trainer* trainer, int totalEpisodes, int alphaBetaTime) {
    printf("Training for %d episodes\n", totalEpisodes);
    int netRoads = 0;
    int netFlats = 0;
    int alphaRoads = 0;
    int alphaFlats = 0;
    int draws = 0;
    bool agentPlaysWhite = true;
    for (int i = 1; i <= totalEpisodes; i++) {
        trainer->learningRate = trainer->learningRate * trainer->learningRateUpdate;
        if (trainer->learningRate < trainer->minLearningRate) {
            trainer->learningRate = trainer->minLearningRate;
        }

        printf("Episode %d: Net Roads: %d, Net Flats: %d, Alpha Roads: %d, Alpha Flats: %d, Draws: %d, LR: %.6f\n",
                i, netRoads, netFlats, alphaRoads, alphaFlats, draws, trainer->learningRate);
        // reset t table
        if (transpositionTable) {
            freeTranspositionTable(transpositionTable);
            transpositionTable = NULL;
        }
        if (monteCarloTable) {
            freeMonteCarloTable(monteCarloTable);
            monteCarloTable = NULL;
        }
        int r = trainEpisodeAlphaBeta(trainer, i, agentPlaysWhite, alphaBetaTime);

        switch (r) {
            case 2:
                netRoads++;
                break;
            case 1:
                netFlats++;
                break;
            case -2:
                alphaRoads++;
                break;
            case -1:
                alphaFlats++;
                break;
            case 0:
                draws++;
                break;
            default:
                break;
        }

        if (i % trainer->saveInterval == 0) {
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_verysmall");
        }
        agentPlaysWhite = !agentPlaysWhite;
    }
    printf("\n");
}

int trainEpisodeAlphaBeta(Trainer* trainer, int episodeNum, bool agentPlaysWhite, int alphaBetaTime) {
    GameState* state = createGameState();

    double** pastStates = (double**)malloc(1000 * sizeof(double*));
    double** pastOutputs = (double**)malloc(1000 * sizeof(double*));
    double* pastValues = (double*)malloc(1000 * sizeof(double));
    int numPastStates = 0;

    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        double* outputs = feedForwardDense(trainer->net, (7 * 36), inputs, 0.0, true);

        pastStates[numPastStates] = inputs;
        pastOutputs[numPastStates] = outputs;
        pastValues[numPastStates] = outputs[0];

        if (numPastStates > 0) {
            double currentValue = pastValues[numPastStates];
            double prevValue = pastValues[numPastStates - 1];
            double immediateReward = pseudoReward(state) - 0.5;

            // Calculate TD error: r + γV(s') - V(s)
            double tdError = immediateReward + (trainer->discountFactor * currentValue) - prevValue;

            double targetValue = prevValue + tdError;

            // Convert back to 0-1 range
            targetValue += 0.5;
            if (targetValue < 0.0) targetValue = 0.0;
            if (targetValue > 1.0) targetValue = 1.0;

            backpropagateDense(trainer->net, pastStates[numPastStates - 1], 
                    pastOutputs[numPastStates - 1], &targetValue, trainer->learningRate);
        }

        GeneratedMoves* moves = generateAllMoves(state, numMoves);
        Move move = moves->moves[0];

        if ((state->turn == WHITE && !agentPlaysWhite) || (state->turn == BLACK && agentPlaysWhite)){
            move = iterativeDeepeningSearch(state, alphaBetaTime);
        } else {
            move = monteCarloGraphSearch(state, trainer->net, true);
        }
        makeMoveNoChecks(state, &move, false);
        freeGeneratedMoves(moves);

        numPastStates++;
    }

    int gameResult = checkGameResult(state);
    int toReturn = 0;

    switch (gameResult) {
        case ROAD_WHITE: toReturn = 2; break;
        case FLAT_WHITE: toReturn = 1; break;
        case ROAD_BLACK: toReturn = -2; break;
        case FLAT_BLACK: toReturn = -1; break;
        case DRAW: toReturn = 0; break;
        default: break;
    }

    if (!agentPlaysWhite) {
        toReturn = -toReturn;
    }

    double finalReward = calculateTimeAdjustedReward(gameResult, numPastStates, 75);

    // Final update for the last state
    if (numPastStates > 0) {
        backpropagateDense(trainer->net, pastStates[numPastStates - 1], 
                pastOutputs[numPastStates - 1], &finalReward, trainer->learningRate);
    }

    // TD(λ) update for all previous states
    double decayFactor = 0.9;  // Lambda for eligibility trace
    for (int i = numPastStates - 2; i >= 0; i--) {
        if (pastStates[i] == NULL || pastOutputs[i] == NULL) {
            fprintf(stderr, "NULL pointer in eligibility trace at index %d\n", i);
            continue;  // Skip this iteration
        }

        double decay = pow(decayFactor * trainer->discountFactor, numPastStates - 1 - i);
        double targetValue = pastValues[i] + (decay * (finalReward - pastValues[i]));

        // Ensure targetValue is in valid range [0, 1]
        if (targetValue < 0.0) targetValue = 0.0;
        if (targetValue > 1.0) targetValue = 1.0;

        backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &targetValue, 
                trainer->learningRate * decay);  // Decay learning rate by eligibility
    }

    for (int i = 0; i < numPastStates; i++) {
        if (pastStates[i]) free(pastStates[i]);
        if (pastOutputs[i]) free(pastOutputs[i]);
    }
    free(pastStates);
    free(pastOutputs);
    free(pastValues);
    freeGameState(state);

    printf("numPastStates: %d, Final reward: %.4f, Result: %d\n", numPastStates, finalReward, toReturn);
    return toReturn;
}

void trainHybrid(Trainer* trainer, int totalEpisodes, int alphaBetaTime) {
    printf("Hybrid training for %d episodes\n", totalEpisodes);
    int regularWhiteRoads = 0;
    int regularWhiteFlats = 0;
    int regularBlackRoads = 0;
    int regularBlackFlats = 0;
    int regularDraws = 0;
    int alphaBetaNetWins = 0;
    int alphaBetaAlphaWins = 0;
    int alphaBetaDraws = 0;
    bool agentPlaysWhite = true;

    for (int i = 1; i <= totalEpisodes; i++) {
        trainer->learningRate = trainer->learningRate * trainer->learningRateUpdate;
        if (trainer->learningRate < trainer->minLearningRate) {
            trainer->learningRate = trainer->minLearningRate;
        }

        printf("Episode %d: Regular: W.Road %d, W.Flat %d, B.Road %d, B.Flat %d, Draw %d | AlphaBeta: Net %d, Alpha %d, Draw %d, LR: %.6f\n",
                i, regularWhiteRoads, regularWhiteFlats, regularBlackRoads, regularBlackFlats, regularDraws,
                alphaBetaNetWins, alphaBetaAlphaWins, alphaBetaDraws, trainer->learningRate);

        if (i % 2 == 1) {
            int r = trainEpisode(trainer, i);
            switch (r) {
                case 2:
                    regularWhiteRoads++;
                    break;
                case 1:
                    regularWhiteFlats++;
                    break;
                case -2:
                    regularBlackRoads++;
                    break;
                case -1:
                    regularBlackFlats++;
                    break;
                case 0:
                    regularDraws++;
                    break;
                default:
                    break;
            }
        } else {
            int r = trainEpisodeAlphaBeta(trainer, i, agentPlaysWhite, alphaBetaTime);
            if (r > 0) {
                alphaBetaNetWins++;
            } else if (r < 0) {
                alphaBetaAlphaWins++;
            } else {
                alphaBetaDraws++;
            }
            agentPlaysWhite = !agentPlaysWhite;
        }

        if (i % trainer->saveInterval == 0) {
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_verysmall");
        }
    }
    printf("\n");
}
