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

double calculateReward(int result, int numMoves, int maxMoves) {
    double baseReward;
    switch (result) {
        case ROAD_WHITE:
            baseReward = 1.0;
            break;
        case FLAT_WHITE:
            baseReward = 0.9;
            break;
        case ROAD_BLACK:
            baseReward = -1.0;
            break;
        case FLAT_BLACK:
            baseReward = -0.9;
            break;
        case DRAW:
            baseReward = 0.0;
            break;
        default:
            baseReward = 0.0;
            break;
    }

    return baseReward;
}

void train(Trainer* trainer, int totalEpisodes) {
    int sock = connectToPython();
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
            clearMonteCarloTable(monteCarloTable);
        }

        int r = trainEpisode(trainer, i, sock);
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
        /* if (i % trainer->saveInterval == 0) { */
        /*     saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_verysmall"); */
        /* } */
    }
    printf("\n");
}

int trainEpisode(Trainer* trainer, int episodeNum, int sock) {
    GameState* state = createGameState();
    // Store the entire sequence of states, actions, and rewards
    double** pastStates = (double**)malloc(1000 * sizeof(double*));
    /* double** pastOutputs = (double**)malloc(1000 * sizeof(double*)); */
    double** pastValues = (double**)malloc(1000 * sizeof(double*));
    int numPastStates = 0;
    MoveList* moves = createMoveList(512);

    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        /* double* outputs = pythonPredict(sock, inputs, (7 * 36)); */

        /* if (numPastStates > 0) { */
        /*     double* reward = malloc(OUTPUT_SIZE * sizeof(double)); */
        /*     memcpy(reward, pastValues[numPastStates - 1], OUTPUT_SIZE * sizeof(double)); */
        /*     // if the output and previous are different by more than 0.1 run TD */
        /*     if (fabs(pastOutputs[numPastStates - 1][0] - outputs[0]) > 0.1) { */
        /*         reward[0] = (outputs[0] + pastOutputs[numPastStates - 1][0]); */
        /*         pythonTrainTD(sock, pastStates[numPastStates - 1], pastOutputs[numPastStates - 1], 1, reward, 7 * 36); */
        /*     } */
        /*  */
        /* } */

        pastStates[numPastStates] = inputs;
        /* pastOutputs[numPastStates] = outputs; */
        pastValues[numPastStates] = malloc(OUTPUT_SIZE * sizeof(double));

        generateAllMoves(state, moves);
        numMoves = moves->numMoves;

        int random = rand() % 10;
        Move move;
        move = monteCarloGraphSearch(state, trainer->net, true, sock, pastValues[numPastStates]);

        makeMoveNoChecks(state, &move, false);
        freeMoveList(moves);

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

    double finalReward = calculateReward(gameResult, numPastStates, 225);

    /* if (state->turn == WHITE) { */
    /*     finalReward = -finalReward; */
    /* } */

    if (numPastStates > 0) {
        pastValues[numPastStates - 1][0] = finalReward;
        /* pythonTrain(sock, pastStates[numPastStates - 1], pastOutputs[numPastStates - 1], 1, pastValues[numPastStates - 1], 7 * 36); */
        pythonTrain(sock, pastStates[numPastStates - 1], NULL, 1, pastValues[numPastStates - 1], 7 * 36);
    }

    for (int i = numPastStates - 2; i >= 0; i--) {
        /* finalReward = -1 * finalReward; */
        double decay = pow(trainer->discountFactor, numPastStates - i - 1);
        pastValues[i][0] = finalReward * decay;
        /* pythonTrain(sock, pastStates[i], pastOutputs[i], 1, pastValues[i], 7 * 36); */
    }

    pythonGameEnd(sock);

    for (int i = 0; i < numPastStates; i++) {
        free(pastStates[i]);
        /* free(pastOutputs[i]); */
    }
    free(pastStates);
    /* free(pastOutputs); */
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

    int sock = connectToPython();

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
            clearTranspositionTable(transpositionTable);
        }
        if (monteCarloTable) {
            clearMonteCarloTable(monteCarloTable);
        }
        int r = trainEpisodeAlphaBeta(trainer, i, agentPlaysWhite, alphaBetaTime, sock);

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

        /* if (i % trainer->saveInterval == 0) { */
        /*     saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_verysmall"); */
        /* } */
        agentPlaysWhite = !agentPlaysWhite;
    }
    printf("\n");
}

int trainEpisodeAlphaBeta(Trainer* trainer, int episodeNum, bool agentPlaysWhite, int alphaBetaTime, int sock) {
    GameState* state = createGameState();

    double** pastStates = (double**)malloc(1000 * sizeof(double*));
    /* double** pastOutputs = (double**)malloc(1000 * sizeof(double*)); */
    double** pastValues = (double**)malloc(1000 * sizeof(double*));
    int numPastStates = 0;
    MoveList* moves = createMoveList(512);

    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        /* double* outputs = pythonPredict(sock, inputs, (7 * 36)); */
        /* printf("Value: %lf\n", outputs[0]); */
        /*  */
        /* if (numPastStates > 0) { */
        /*     double* reward = malloc(OUTPUT_SIZE * sizeof(double)); */
        /*     memcpy(reward, pastValues[numPastStates - 1], OUTPUT_SIZE * sizeof(double)); */
        /*     if (fabs(pastOutputs[numPastStates - 1][0] - outputs[0]) > 0.1) { */
        /*         reward[0] = (outputs[0] + pastOutputs[numPastStates - 1][0]); */
        /*         pythonTrainTD(sock, pastStates[numPastStates - 1], pastOutputs[numPastStates - 1], 1, reward, 7 * 36); */
        /*     } */
        /* } */
        /*  */

        pastStates[numPastStates] = inputs;
        /* pastOutputs[numPastStates] = outputs; */
        pastValues[numPastStates] = malloc(OUTPUT_SIZE * sizeof(double));

        generateAllMoves(state, moves);
        /* printf("Got moves\n"); */
        Move move = moves->moves[0];

        if ((state->turn == WHITE && !agentPlaysWhite) || (state->turn == BLACK && agentPlaysWhite)){
            // run monte carlo anyway so we can get the policy values
            move = monteCarloGraphSearch(state, trainer->net, true, sock, pastValues[numPastStates]);

            int random = rand() % 10;
            if (random < 9) {
                move = iterativeDeepeningSearch(state, alphaBetaTime);
            } 
        } else {
            /* move = monteCarloGraphSearch(state, trainer->net, false, sock, pastValues[numPastStates]); */
            move = monteCarloGraphSearch(state, trainer->net, true, sock, pastValues[numPastStates]);
            int random = rand() % 10;
            if (random < 6) {
                /* move = iterativeDeepeningSearch(state, alphaBetaTime); */
                move = monteCarloGraphSearch(state, trainer->net, false, sock, pastValues[numPastStates]);
            } 
        }
        makeMoveNoChecks(state, &move, false);

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

    double finalReward = calculateReward(gameResult, numPastStates, 75);

    /* if (state->turn == WHITE) { */
    /*     finalReward = -finalReward; */
    /* } */

    // Final update for the last state
    if (numPastStates > 0) {
        /* backpropagateDense(trainer->net, pastStates[numPastStates - 1],  */
                /* pastOutputs[numPastStates - 1], &finalReward, trainer->learningRate); */
        pastValues[numPastStates - 1][0] = finalReward;
        /* pythonTrain(sock, pastStates[numPastStates - 1], pastOutputs[numPastStates - 1], 1, pastValues[numPastStates - 1], 7 * 36); */
        pythonTrain(sock, pastStates[numPastStates - 1], NULL, 1, pastValues[numPastStates - 1], 7 * 36);

    }

    for (int i = numPastStates - 2; i >= 0; i--) {
        /* finalReward = -1 * finalReward; */
        /* finalReward = finalReward * trainer->discountFactor; */
        double decay = pow(trainer->discountFactor, numPastStates - i - 1);
        pastValues[i][0] = finalReward * decay;
        /* if (pastStates[i] == NULL || pastOutputs[i] == NULL) { */
        /*     fprintf(stderr, "NULL pointer in eligibility trace at index %d\n", i); */
        /*     continue;  // Skip this iteration */
        /* } */


        /* backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &targetValue, trainer->learningRate * decay); */
        /* pythonTrain(sock, pastStates[i], pastOutputs[i], 1, pastValues[i], 7 * 36); */
        pythonTrain(sock, pastStates[i], NULL, 1, pastValues[i], 7 * 36);
    }

    pythonGameEnd(sock);

    for (int i = 0; i < numPastStates; i++) {
        if (pastStates[i]) free(pastStates[i]);
        /* if (pastOutputs[i]) free(pastOutputs[i]); */
        if (pastValues[i]) free(pastValues[i]);
    }
    free(pastStates);
    /* free(pastOutputs); */
    free(pastValues);
    freeGameState(state);
    freeMoveList(moves);

    printf("numPastStates: %d, Final reward: %.4f, Result: %d\n", numPastStates, finalReward, toReturn);
    return toReturn;
}

void trainHybrid(Trainer* trainer, int totalEpisodes, int alphaBetaTime) {
    printf("Hybrid training for %d episodes\n", totalEpisodes);
    int sock = connectToPython();
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

        if (transpositionTable) {
            clearTranspositionTable(transpositionTable);
        }
        if (monteCarloTable) {
            clearMonteCarloTable(monteCarloTable);
        }
        if (i % 2 == 1) {
            int r = trainEpisode(trainer, i, sock);
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
            int r = trainEpisodeAlphaBeta(trainer, i, agentPlaysWhite, alphaBetaTime, sock);
            if (r > 0) {
                alphaBetaNetWins++;
            } else if (r < 0) {
                alphaBetaAlphaWins++;
            } else {
                alphaBetaDraws++;
            }
            agentPlaysWhite = !agentPlaysWhite;
        }

        /* if (i % trainer->saveInterval == 0) { */
        /*     saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_verysmall"); */
        /* } */
    }
    printf("\n");
}
