#ifndef NEURAL_NET_TRAINER_H
#define NEURAL_NET_TRAINER_H

#include "neuralNetworks.h"
#include "../lib/board.h"
#include "../lib/moves.h"
#include "eval.h"
#include "searches.h"
#include "monteCarloGraphSearch.h"
#include "pythonTrainer.h"

typedef struct {
    DenseNeuralNet* net;
    double learningRateUpdate;
    double minLearningRate;
    double learningRate;
    int saveInterval;
    double discountFactor;
} Trainer;

Trainer* createTrainer(DenseNeuralNet* net, double learningRateUpdate, double minLearningRate, double learningRate, int saveInterval, double discountFactor);
void freeTrainer(Trainer* trainer);

double pseudoReward(const GameState* state);

void train(Trainer* trainer, int totalEpisodes);
int trainEpisode(Trainer* trainer, int episodeNum, int sock);

void trainAlphaBeta(Trainer* trainer, int totalEpisodes, int alphaBetaTime);
int trainEpisodeAlphaBeta(Trainer* trainer, int episodeNum, bool agentPlaysWhite, int alphaBetaTime, int sock);

void trainHybrid(Trainer* trainer, int totalEpisodes, int alphaBetaTime);

#endif // NEURAL_NET_TRAINER_H
