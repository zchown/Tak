#include "neuralNetTrainer.h"

Trainer* createTrainer(DenseNeuralNet* net, double epsilonDecay, double minEpsilon, int saveInterval) {
    Trainer* trainer = (Trainer*)malloc(sizeof(Trainer));
    trainer->net = net;
    trainer->epsilonDecay = epsilonDecay;
    trainer->minEpsilon = minEpsilon;
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
    for (int i = 1; i <= totalEpisodes; i++) {
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
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_3");
        }

    }
    printf("\n");
}

int trainEpisode(Trainer* trainer, int episodeNum) {
    GameState* state = createGameState();

    double** pastStates = (double**)malloc(1000 * sizeof(double*));
    double** pastOutputs = (double**)malloc(1000 * sizeof(double*));
    int numPastStates = 0;

    double epsilon = fmax(trainer->minEpsilon, 1.0 - (episodeNum * trainer->epsilonDecay));
    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        double* outputs = feedForwardDense(trainer->net, (7 * 36), inputs, 0.0);
        pastStates[numPastStates] = inputs;
        pastOutputs[numPastStates] = outputs;
        numPastStates++;
        double pReward = pseudoReward(state);

        backpropagateDense(trainer->net, inputs, outputs, &pReward, 0.15);

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

    double reward = 0;
    int toReturn = 0;
    switch (checkGameResult(state)) {
        case ROAD_WHITE:
            reward = 1.0;
            toReturn = 2;
            break;
        case FLAT_WHITE:
            reward = 0.8;
            toReturn = 1;
            break;
        case ROAD_BLACK:
            reward = 0.0;
            toReturn = -2;
            break;
        case FLAT_BLACK:
            reward = 0.2;
            toReturn = -1;
            break;
        case DRAW:
            reward = 0.0;
            break;
        default:
            break;
    }

    for (int i = 0; i < numPastStates; i++) {
        backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &reward, 0.15);
    }

    for (int i = 0; i < numPastStates; i++) {
        free(pastStates[i]);
        free(pastOutputs[i]);
    }
    free(pastStates);
    free(pastOutputs);
    printf("numPastStates: %d\n", numPastStates);
    return toReturn;
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
                    vector[i * (BOARD_SIZE + 1) + j] = 0.7;
                } else {
                    vector[i * (BOARD_SIZE + 1) + j] = 0.9;
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

void trainAlphaBeta(Trainer* trainer, int totalEpisodes, int alphaBetaTime) {
    printf("Training for %d episodes\n", totalEpisodes);
    int netRoads = 0;
    int netFlats = 0;
    int alphaRoads = 0;
    int alphaFlats = 0;
    int draws = 0;
    bool agentPlaysWhite = true;
    for (int i = 1; i <= totalEpisodes; i++) {
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
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_3");
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

    double epsilon = fmax(trainer->minEpsilon, 0.5 - (episodeNum * trainer->epsilonDecay)) * 1000;
    epsilon = 15;
    int numMoves = 512;
    while (checkGameResult(state) == CONTINUE) {
        double* inputs = gameStateToVector(state);
        double* outputs = feedForwardDense(trainer->net, (7 * 36), inputs, 0.0);
        pastStates[numPastStates] = inputs;
        pastOutputs[numPastStates] = outputs;
        numPastStates++;
        double pReward = pseudoReward(state);

        backpropagateDense(trainer->net, inputs, outputs, &pReward, 0.15);

        GeneratedMoves* moves = generateAllMoves(state, numMoves);
        Move move = moves->moves[rand() % moves->numMoves];
        if ((state->turn == WHITE && !agentPlaysWhite) || (state->turn == BLACK && agentPlaysWhite)){
            if ((rand() % 100 < epsilon)) {
                move = moves->moves[rand() % moves->numMoves];
            } else {
                move = iterativeDeepeningSearch(state, alphaBetaTime);
            }
        } else {
            numMoves = moves->numMoves;

            if ((rand() % 100) < epsilon) {
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

    double reward = 0;
    int toReturn = 0;
    printf("Game result: %d\n", checkGameResult(state));
    switch (checkGameResult(state)) {
        case ROAD_WHITE:
            reward = 1.0;
            toReturn = 2;
            break;
        case FLAT_WHITE:
            reward = 0.8;
            toReturn = 1;
            break;
        case ROAD_BLACK:
            reward = 0.0;
            toReturn = -2;
            break;
        case FLAT_BLACK:
            reward = 0.2;
            toReturn = -1;
            break;
        case DRAW:
            reward = 0.5;
            toReturn = 0;
            break;
        default:
            break;
    }

    if (!agentPlaysWhite) {
        toReturn *= -1;
    }

    for (int i = 0; i < numPastStates; i++) {
        backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &reward, 0.15);
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
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_3");
        }
    }
    printf("\n");
}
