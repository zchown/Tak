#include "neuralNetTrainer.h"

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

double calculateTimeAdjustedReward(int result, int numMoves, int maxMoves) {
    double baseReward;
    switch (result) {
        case ROAD_WHITE:
            baseReward = 1.0;
            break;
        case FLAT_WHITE:
            baseReward = 0.6;
            break;
        case ROAD_BLACK:
            baseReward = 0.0;
            break;
        case FLAT_BLACK:
            baseReward = 0.4;
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

    if (baseReward > 0.5) {  // Win for White
        toReturn = baseReward + (moveFactor * 0.5);
        if (toReturn < 0.5) {
            toReturn = 0.5;
        }
    } else if (baseReward < 0.5) {  
        toReturn = baseReward - (moveFactor * 0.5);
        if (toReturn > 0.5) {
            toReturn = 0.5;
        }
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
        printf("Episode %d: White Roads: %d, White Flats: %d, Black Roads: %d, Black Flats: %d, Draws: %d\n", 
                i, whiteRoads, whiteFlats, blackRoads, blackFlats, draws);
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
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_large");
        }

    }
    printf("\n");
}

int trainEpisode(Trainer* trainer, int episodeNum) {
    GameState* state = createGameState();

    double** pastStates = (double**)malloc(1000 * sizeof(double*));
    double** pastOutputs = (double**)malloc(1000 * sizeof(double*));
    int numPastStates = 0;

    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        double* outputs = feedForwardDense(trainer->net, (7 * 36), inputs, 0.0);
        pastStates[numPastStates] = inputs;
        pastOutputs[numPastStates] = outputs;
        numPastStates++;
        double pReward = pseudoReward(state);

        /* pReward = meanSquaredError(outputs[0], pReward); */

        backpropagateDense(trainer->net, inputs, outputs, &pReward, trainer->learningRate);

        GeneratedMoves* moves = generateAllMoves(state, numMoves);
        numMoves = moves->numMoves;

        Move move = moves->moves[rand() % moves->numMoves];

        if ((rand() % 100) < 1) {
            move = moves->moves[rand() % moves->numMoves];
        } else {
            double bestValue = -INFINITY;
            if (state->turn == BLACK) {
                bestValue = INFINITY;
            }
            for (int i = 0; i < moves->numMoves; i++) {
                Move curMove = moves->moves[i];
                makeMoveNoChecks(state, &curMove, false);

                double* gameVector = gameStateToVector(state);
                double* gameOutputs = feedForwardDense(trainer->net, (7 * 36), gameVector, 0.0);
                if ((gameOutputs[0] > bestValue && state->turn == WHITE) || (gameOutputs[0] < bestValue && state->turn == BLACK)) {
                    bestValue = gameOutputs[0];
                    move = curMove;
                }

                undoMoveNoChecks(state, &curMove, false);
                free(gameVector);
                free(gameOutputs);
            }
        }

        makeMoveNoChecks(state, &move, false);
        freeGeneratedMoves(moves);
    }

    int gameResult = checkGameResult(state);
    int toReturn = 0;
    
    // Map the game result to the return value
    switch (gameResult) {
        case ROAD_WHITE: toReturn = 2; break;
        case FLAT_WHITE: toReturn = 1; break;
        case ROAD_BLACK: toReturn = -2; break;
        case FLAT_BLACK: toReturn = -1; break;
        case DRAW: toReturn = 0; break;
        default: break;
    }
    
    double reward = calculateTimeAdjustedReward(gameResult, numPastStates, 225);

    for (int i = 0; i < numPastStates; i++) {
        feedForwardDense(trainer->net, (7 * 36), pastStates[i], 0.0);
        backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &reward, trainer->learningRate);
    }

    for (int i = 0; i < numPastStates; i++) {
        free(pastStates[i]);
        free(pastOutputs[i]);
    }
    free(pastStates);
    free(pastOutputs);
    freeGameState(state);
    printf("numPastStates: %d\n", numPastStates);
    return toReturn;
}


double pseudoReward(const GameState* state) {
    /* double toReturn = (__builtin_popcount(state->whiteControlled)  */
            /* - __builtin_popcount(state->blackControlled) - KOMI) / 36.0; */
    int connectivity = connectivityIndex(state);
    double toReturn = 0;
    if (connectivity > 0) {
        toReturn += 0.4;
    } else if (connectivity < 0) {
        toReturn -= 0.4;
    }
    toReturn += 0.5;
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
        printf("Episode %d: Net Roads: %d, Net Flats: %d, Alpha Roads: %d, Alpha Flats: %d, Draws: %d\n",
               i, netRoads, netFlats, alphaRoads, alphaFlats, draws);
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
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_large");
        }
        agentPlaysWhite = !agentPlaysWhite;
    }
    printf("\n");
}

int trainEpisodeAlphaBeta(Trainer* trainer, int episodeNum, bool agentPlaysWhite, int alphaBetaTime) {
    GameState* state = createGameState();

    double** pastStates = (double**)malloc(1000 * sizeof(double*));
    double** pastOutputs = (double**)malloc(1000 * sizeof(double*));
    int numPastStates = 0;

    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        double* outputs = feedForwardDense(trainer->net, (7 * 36), inputs, 0.0);
        pastStates[numPastStates] = inputs;
        pastOutputs[numPastStates] = outputs;
        numPastStates++;
        double pReward = pseudoReward(state);

        /* pReward = meanSquaredError(outputs[0], pReward); */
        backpropagateDense(trainer->net, inputs, outputs, &pReward, trainer->learningRate);

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
                double bestValue = -INFINITY;
                if (!agentPlaysWhite) {
                    bestValue = INFINITY;
                }
                for (int i = 0; i < moves->numMoves; i++) {
                    Move curMove = moves->moves[i];
                    makeMoveNoChecks(state, &curMove, false);

                    double* gameVector = gameStateToVector(state);
                    double* gameOutputs = feedForwardDense(trainer->net, (7 * 36), gameVector, 0.0);

                    if (agentPlaysWhite && gameOutputs[0] > bestValue) {
                        bestValue = gameOutputs[0];
                        move = curMove;
                    } else if (!agentPlaysWhite && gameOutputs[0] < bestValue) {
                        bestValue = gameOutputs[0];
                        move = curMove;
                    }

                    undoMoveNoChecks(state, &curMove, false);
                    free(gameVector);
                    free(gameOutputs);
                }
                freeGeneratedMoves(moves);
            }

        }

        makeMoveNoChecks(state, &move, false);
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

    double reward = calculateTimeAdjustedReward(gameResult, numPastStates, 100);

    /* if (!agentPlaysWhite) { */
        /* toReturn *= -1; */
        /* reward = 1.0 - reward; */
    /* } */

    for (int i = 0; i < numPastStates; i++) {
        /* double treward = meanSquaredError(pastOutputs[i][0], reward); */
        feedForwardDense(trainer->net, (7 * 36), pastStates[i], 0.0);
        backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &reward, trainer->learningRate);
    }

    for (int i = 0; i < numPastStates; i++) {
        free(pastStates[i]);
        free(pastOutputs[i]);
    }
    free(pastStates);
    free(pastOutputs);
    freeGameState(state);
    printf("numPastStates: %d\n", numPastStates);
    printf("Reward: %f\n", reward);
    printf("To return: %d\n", toReturn);
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
        printf("Episode %d: Regular: W.Road %d, W.Flat %d, B.Road %d, B.Flat %d, Draw %d | AlphaBeta: Net %d, Alpha %d, Draw %d\n",
               i, regularWhiteRoads, regularWhiteFlats, regularBlackRoads, regularBlackFlats, regularDraws,
               alphaBetaNetWins, alphaBetaAlphaWins, alphaBetaDraws);

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
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_large");
        }
    }
    printf("\n");
}
