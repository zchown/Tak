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
            saveDenseNeuralNet(trainer->net, "n_models/tak_model.weights_2");
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

        backpropagateDense(trainer->net, inputs, outputs, &pReward, epsilon);

        GeneratedMoves* moves = generateAllMoves(state, numMoves);
        numMoves = moves->numMoves;

        Move move = (Move){0};

        if ((rand() % 100) < 15) {
            move = moves->moves[rand() % moves->numMoves];
        } else {
            // Select the best move based on neural network evaluation
            double bestValue = -INFINITY;
            for (int i = 0; i < moves->numMoves; i++) {
                Move curMove = moves->moves[i];
                makeMoveNoChecks(state, &curMove, false);

                double* gameVector = gameStateToVector(state);
                double* gameOutputs = feedForwardDense(trainer->net, (7 * 36), gameVector, 0.0);
                if (gameOutputs[0] > bestValue) {
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
            reward = 0.5;
            toReturn = 1;
            break;
        case ROAD_BLACK:
            reward = -1.0;
            toReturn = -2;
            break;
        case FLAT_BLACK:
            reward = -0.5;
            toReturn = -1;
            break;
        case DRAW:
            reward = 0.0;
            break;
        default:
            break;
    }

    for (int i = 0; i < numPastStates; i++) {
        backpropagateDense(trainer->net, pastStates[i], pastOutputs[i], &reward, epsilon);
    }

    for (int i = 0; i < numPastStates; i++) {
        free(pastStates[i]);
        free(pastOutputs[i]);
    }
    free(pastStates);
    free(pastOutputs);
    return toReturn;
}

