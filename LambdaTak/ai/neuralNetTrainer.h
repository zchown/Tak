#ifndef NEURAL_NET_TRAINER_H
#define NEURAL_NET_TRAINER_H

#include "neuralNetworks.h"
#include "../lib/board.h"
#include "../lib/moves.h"
#include "eval.h"
#include "searches.h"

typedef struct {
    DenseNeuralNet* net;
    double epsilonDecay;
    double minEpsilon;
    int saveInterval;
} Trainer;

Trainer* createTrainer(DenseNeuralNet* net, double epsilonDecay, double minEpsilon, int saveInterval);
void freeTrainer(Trainer* trainer);

double* gameStateToVector(const GameState* state);
double pseudoReward(const GameState* state);

void train(Trainer* trainer, int totalEpisodes);
int trainEpisode(Trainer* trainer, int episodeNum);

void trainAlphaBeta(Trainer* trainer, int totalEpisodes, int alphaBetaTime);
int trainEpisodeAlphaBeta(Trainer* trainer, int episodeNum, bool agentPlaysWhite, int alphaBetaTime);

void trainHybrid(Trainer* trainer, int totalEpisodes, int alphaBetaTime);

#endif // NEURAL_NET_TRAINER_H
