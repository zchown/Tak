#include "neuralNetTrainer.h"
#include <omp.h>

Trainer* createTrainer(DenseNeuralNet* net, double learningRateUpdate, double minLearningRate, double learningRate, int saveInterval) {
    Trainer* trainer = (Trainer*)malloc(sizeof(Trainer));
    trainer->net = net;
    trainer->learningRateUpdate = learningRateUpdate;
    trainer->minLearningRate = minLearningRate;
    trainer->learningRate = learningRate;
    trainer->saveInterval = saveInterval;
    return trainer;
}

void freeTrainer(Trainer* trainer) {
    free(trainer);
}

void train(Trainer* trainer, int totalEpisodes) {
    printf("Training for %d episodes\n", totalEpisodes);
    int whiteRoads = 0;
    int whiteFlats = 0;
    int blackRoads = 0;
    int blackFlats = 0;
    int draws = 0;

    // Use OpenMP to parallelize the episodes
#pragma omp parallel for reduction(+:whiteRoads,whiteFlats,blackRoads,blackFlats,draws) schedule(dynamic)
    for (int i = 1; i <= totalEpisodes; i++) {
        double localLearningRate = trainer->learningRate * pow(trainer->learningRateUpdate, i-1);

        if (i % 10 == 0) {
#pragma omp critical(print_status)
            {
                printf("Episode %d: White Roads: %d, White Flats: %d, Black Roads: %d, Black Flats: %d, Draws: %d\n", 
                        i, whiteRoads, whiteFlats, blackRoads, blackFlats, draws);
            }
        }

        int r = trainEpisode(trainer, i, localLearningRate);
        switch (r) {
            case 2: whiteRoads++; break;
            case 1: whiteFlats++; break;
            case -2: blackRoads++; break;
            case -1: blackFlats++; break;
            case 0: draws++; break;
            default: break;
        }

        if (i % trainer->saveInterval == 0) {
#pragma omp critical(save_model)
            {
                saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_small");
            }
        }
    }

    printf("\nFinal results: White Roads: %d, White Flats: %d, Black Roads: %d, Black Flats: %d, Draws: %d\n", 
            whiteRoads, whiteFlats, blackRoads, blackFlats, draws);
}

int trainEpisode(Trainer* trainer, int episodeNum, double localLearningRate) {
    GameState* state = createGameState();

    double** pastStates = (double**)malloc(1000 * sizeof(double*));
    double** pastOutputs = (double**)malloc(1000 * sizeof(double*));
    int numPastStates = 0;

    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        double* outputs;

#pragma omp critical(neural_network)
        {
            outputs = feedForwardDense(trainer->net, (7 * 36), inputs, 0.0);
        }

        pastStates[numPastStates] = inputs;
        pastOutputs[numPastStates] = outputs;
        numPastStates++;
        double pReward = pseudoReward(state);

        pReward = meanSquaredError(outputs[0], pReward);

#pragma omp critical(neural_network)
        {
            backpropagateDense(trainer->net, inputs, outputs, &pReward, localLearningRate);
        }

        GeneratedMoves* moves = generateAllMoves(state, numMoves);
        numMoves = moves->numMoves;

        Move move = moves->moves[rand() % moves->numMoves];

        if ((rand() % 100) < 15) {
            move = moves->moves[rand() % moves->numMoves];
        } else {
            double bestValue = -INFINITY;
            if (state->turn == BLACK) {
                bestValue = INFINITY;
            }

#pragma omp parallel
            {
                double threadBestValue = (state->turn == WHITE) ? -INFINITY : INFINITY;
                Move threadBestMove = move;

#pragma omp for nowait
                for (int i = 0; i < moves->numMoves; i++) {
                    Move curMove = moves->moves[i];

                    GameState* localState = copyGameState(state);
                    makeMoveNoChecks(localState, &curMove, false);

                    double* gameVector = gameStateToVector(localState);
                    double* gameOutputs;

#pragma omp critical(neural_network)
                    {
                        gameOutputs = feedForwardDense(trainer->net, (7 * 36), gameVector, 0.0);
                    }

                    bool isBetter = false;
                    if (state->turn == WHITE) {
                        isBetter = gameOutputs[0] > threadBestValue;
                    } else {
                        isBetter = gameOutputs[0] < threadBestValue;
                    }

                    if (isBetter) {
                        threadBestValue = gameOutputs[0];
                        threadBestMove = curMove;
                    }

                    freeGameState(localState);
                    free(gameVector);
                    free(gameOutputs);
                }

#pragma omp critical(move_selection)
                {
                    bool isBetter = false;
                    if (state->turn == WHITE) {
                        isBetter = threadBestValue > bestValue;
                    } else {
                        isBetter = threadBestValue < bestValue;
                    }

                    if (isBetter) {
                        bestValue = threadBestValue;
                        move = threadBestMove;
                    }
                }
            }
        }

        makeMoveNoChecks(state, &move, false);
        freeGeneratedMoves(moves);
    }

    double reward = 0;
    int toReturn = 0;
    switch (checkGameResult(state)) {
        case ROAD_WHITE: reward = 1.0; toReturn = 2; break;
        case FLAT_WHITE: reward = 0.8; toReturn = 1; break;
        case ROAD_BLACK: reward = 0.0; toReturn = -2; break;
        case FLAT_BLACK: reward = 0.2; toReturn = -1; break;
        case DRAW: reward = 0.5; break;
        default: break;
    }

#pragma omp parallel for
    for (int i = 0; i < numPastStates; i++) {
        double localReward = reward;
        localReward = meanSquaredError(pastOutputs[i][0], localReward);

        double* localOutputs;

#pragma omp critical(neural_network)
        {
            localOutputs = feedForwardDense(trainer->net, (7 * 36), pastStates[i], 0.0);
            backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &localReward, localLearningRate);
        }

        free(localOutputs);
    }

    for (int i = 0; i < numPastStates; i++) {
        free(pastStates[i]);
        free(pastOutputs[i]);
    }
    free(pastStates);
    free(pastOutputs);
    freeGameState(state);

    return toReturn;
}

void trainAlphaBeta(Trainer* trainer, int totalEpisodes, int alphaBetaTime) {
    printf("Training for %d episodes\n", totalEpisodes);
    int netRoads = 0;
    int netFlats = 0;
    int alphaRoads = 0;
    int alphaFlats = 0;
    int draws = 0;

#pragma omp parallel for reduction(+:netRoads,netFlats,alphaRoads,alphaFlats,draws) schedule(dynamic)
    for (int i = 1; i <= totalEpisodes; i++) {
        bool agentPlaysWhite = (i % 2 == 1);  
        double localLearningRate = trainer->learningRate * pow(trainer->learningRateUpdate, i-1);

        if (i % 10 == 0) {
#pragma omp critical(print_status)
            {
                printf("Episode %d: Net Roads: %d, Net Flats: %d, Alpha Roads: %d, Alpha Flats: %d, Draws: %d\n",
                        i, netRoads, netFlats, alphaRoads, alphaFlats, draws);
            }
        }

        int r = trainEpisodeAlphaBeta(trainer, i, agentPlaysWhite, alphaBetaTime, localLearningRate);

        switch (r) {
            case 2: netRoads++; break;
            case 1: netFlats++; break;
            case -2: alphaRoads++; break;
            case -1: alphaFlats++; break;
            case 0: draws++; break;
            default: break;
        }

        if (i % trainer->saveInterval == 0) {
#pragma omp critical(save_model)
            {
                saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_small");
            }
        }
    }

    printf("\nFinal results: Net Roads: %d, Net Flats: %d, Alpha Roads: %d, Alpha Flats: %d, Draws: %d\n",
            netRoads, netFlats, alphaRoads, alphaFlats, draws);
}

int trainEpisodeAlphaBeta(Trainer* trainer, int episodeNum, bool agentPlaysWhite, int alphaBetaTime, double localLearningRate) {
    GameState* state = createGameState();

    double** pastStates = (double**)malloc(1000 * sizeof(double*));
    double** pastOutputs = (double**)malloc(1000 * sizeof(double*));
    int numPastStates = 0;

    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        double* outputs;

#pragma omp critical(neural_network)
        {
            outputs = feedForwardDense(trainer->net, (7 * 36), inputs, 0.0);
        }

        pastStates[numPastStates] = inputs;
        pastOutputs[numPastStates] = outputs;
        numPastStates++;
        double pReward = pseudoReward(state);

        pReward = meanSquaredError(outputs[0], pReward);

#pragma omp critical(neural_network)
        {
            backpropagateDense(trainer->net, inputs, outputs, &pReward, localLearningRate);
        }

        GeneratedMoves* moves = generateAllMoves(state, numMoves);
        Move move = moves->moves[rand() % moves->numMoves];

        if ((state->turn == WHITE && !agentPlaysWhite) || (state->turn == BLACK && agentPlaysWhite)){
            if ((rand() % 100 < 10)) {
                move = moves->moves[rand() % moves->numMoves];
            } else {
                move = iterativeDeepeningSearch(state, alphaBetaTime);
            }
        } else {
            numMoves = moves->numMoves;

            if ((rand() % 100) < 10) {
                move = moves->moves[rand() % moves->numMoves];
            } else {
                double bestValue = agentPlaysWhite ? -INFINITY : INFINITY;

#pragma omp parallel
                {
                    double threadBestValue = agentPlaysWhite ? -INFINITY : INFINITY;
                    Move threadBestMove = move;

#pragma omp for nowait
                    for (int i = 0; i < moves->numMoves; i++) {
                        Move curMove = moves->moves[i];

                        GameState* localState = copyGameState(state);
                        makeMoveNoChecks(localState, &curMove, false);

                        double* gameVector = gameStateToVector(localState);
                        double* gameOutputs;

#pragma omp critical(neural_network)
                        {
                            gameOutputs = feedForwardDense(trainer->net, (7 * 36), gameVector, 0.0);
                        }

                        bool isBetter = false;
                        if (agentPlaysWhite && gameOutputs[0] > threadBestValue) {
                            threadBestValue = gameOutputs[0];
                            threadBestMove = curMove;
                            isBetter = true;
                        } else if (!agentPlaysWhite && gameOutputs[0] < threadBestValue) {
                            threadBestValue = gameOutputs[0];
                            threadBestMove = curMove;
                            isBetter = true;
                        }

                        freeGameState(localState);
                        free(gameVector);
                        free(gameOutputs);
                    }

#pragma omp critical(move_selection)
                    {
                        bool isBetter = false;
                        if (agentPlaysWhite && threadBestValue > bestValue) {
                            isBetter = true;
                        } else if (!agentPlaysWhite && threadBestValue < bestValue) {
                            isBetter = true;
                        }

                        if (isBetter) {
                            bestValue = threadBestValue;
                            move = threadBestMove;
                        }
                    }
                }
            }
        }

        makeMoveNoChecks(state, &move, false);
        freeGeneratedMoves(moves);
    }

    double reward = 0;
    int toReturn = 0;

    switch (checkGameResult(state)) {
        case ROAD_WHITE: reward = 1.0; toReturn = 2; break;
        case FLAT_WHITE: reward = 0.8; toReturn = 1; break;
        case ROAD_BLACK: reward = 0.0; toReturn = -2; break;
        case FLAT_BLACK: reward = 0.2; toReturn = -1; break;
        case DRAW: reward = 0.5; toReturn = 0; break;
        default: break;
    }

    if (!agentPlaysWhite) {
        toReturn *= -1;
    }

#pragma omp parallel for
    for (int i = 0; i < numPastStates; i++) {
        double localReward = reward;
        localReward = meanSquaredError(pastOutputs[i][0], localReward);

        double* localOutputs;

#pragma omp critical(neural_network)
        {
            localOutputs = feedForwardDense(trainer->net, (7 * 36), pastStates[i], 0.0);
            backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &localReward, localLearningRate);
        }

        free(localOutputs);
    }

    for (int i = 0; i < numPastStates; i++) {
        free(pastStates[i]);
        free(pastOutputs[i]);
    }
    free(pastStates);
    free(pastOutputs);
    freeGameState(state);

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

#pragma omp parallel for reduction(+:regularWhiteRoads,regularWhiteFlats,regularBlackRoads,regularBlackFlats,regularDraws,alphaBetaNetWins,alphaBetaAlphaWins,alphaBetaDraws) schedule(dynamic)
    for (int i = 1; i <= totalEpisodes; i++) {
        bool agentPlaysWhite = (i % 4 < 2);  
        double localLearningRate = trainer->learningRate * pow(trainer->learningRateUpdate, i-1);

        if (i % 10 == 0) {
#pragma omp critical(print_status)
            {
                printf("Episode %d: Regular: W.Road %d, W.Flat %d, B.Road %d, B.Flat %d, Draw %d | AlphaBeta: Net %d, Alpha %d, Draw %d\n",
                        i, regularWhiteRoads, regularWhiteFlats, regularBlackRoads, regularBlackFlats, regularDraws,
                        alphaBetaNetWins, alphaBetaAlphaWins, alphaBetaDraws);
            }
        }

        if (i % 2 == 1) {
            int r = trainEpisode(trainer, i, localLearningRate);
            switch (r) {
                case 2: regularWhiteRoads++; break;
                case 1: regularWhiteFlats++; break;
                case -2: regularBlackRoads++; break;
                case -1: regularBlackFlats++; break;
                case 0: regularDraws++; break;
                default: break;
            }
        } else {
            int r = trainEpisodeAlphaBeta(trainer, i, agentPlaysWhite, alphaBetaTime, localLearningRate);
            if (r > 0) {
                alphaBetaNetWins++;
            } else if (r < 0) {
                alphaBetaAlphaWins++;
            } else {
                alphaBetaDraws++;
            }
        }

        if (i % trainer->saveInterval == 0) {
#pragma omp critical(save_model)
            {
                saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_small");
            }
        }
    }

    printf("\nFinal results - Regular: W.Road %d, W.Flat %d, B.Road %d, B.Flat %d, Draw %d | AlphaBeta: Net %d, Alpha %d, Draw %d\n",
            regularWhiteRoads, regularWhiteFlats, regularBlackRoads, regularBlackFlats, regularDraws,
            alphaBetaNetWins, alphaBetaAlphaWins, alphaBetaDraws);
}

double pseudoReward(const GameState* state) {
    double toReturn = (__builtin_popcount(state->whiteControlled) 
            - __builtin_popcount(state->blackControlled) - KOMI) / 36.0;
    int connectivity = connectivityIndex(state);
    if (connectivity > 0) {
        toReturn += 0.1;
    } else if (connectivity < 0) {
        toReturn -= 0.1;
    }
    toReturn += 0.5;
    if (toReturn > 1.0) {
        toReturn = 1.0;
    } else if (toReturn < 0) {
        toReturn = 0.0;
    }
    return toReturn;
}

// 0-1 normalized
// -1-1 was giving me issues previously
double* gameStateToVector(const GameState* state) {
    // top 7 pieces for each square
    double* vector = (double*)malloc(TOTAL_SQUARES * (BOARD_SIZE + 1) * sizeof(double));
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        Square sq = state->board->squares[i];
        int curIndex = sq.numPieces - 1;
        for (int j = 0; j < (BOARD_SIZE + 1); j++) {
            if (curIndex >= 0) {
                if (sq.pieces[curIndex].stone == FLAT) {
                    vector[i * (BOARD_SIZE + 1) + j] = 0.8;
                } else if (sq.pieces[curIndex].stone == STANDING) {
                    vector[i * (BOARD_SIZE + 1) + j] = 0.6;
                } else {
                    vector[i * (BOARD_SIZE + 1) + j] = 0.0;
                }
                if (sq.pieces[curIndex].color == BLACK) {
                    vector[i * (BOARD_SIZE + 1) + j] = 1.0 - vector[i * (BOARD_SIZE + 1) + j];
                }
                curIndex--;
            } else {
                vector[i * (BOARD_SIZE + 1) + j] = 0.0;
            }
        }

    }
    return vector;
}
