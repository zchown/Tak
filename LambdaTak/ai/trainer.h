#ifndef TRAINER_H
#define TRAINER_H

#include "qlearner.h"
#include "../lib/board.h"
#include "../lib/moves.h"
#include "searches.h"
#include <stdio.h>
#include <time.h>

#define MAX_EPISODES 10000
#define WIN_REWARD 1.0
#define LOSS_REWARD -1.0
#define DRAW_REWARD 0.5
#define STEP_PENALTY -0.01

typedef struct {
    QLearningAgent* agent;
    double epsilonDecay;
    double minEpsilon;
    int saveInterval;
} Trainer;

Trainer* createTrainer(QLearningAgent* agent, double epsilonDecay, double minEpsilon, int saveInterval);
void freeTrainer(Trainer* trainer);

void train(Trainer* trainer, int totalEpisodes);
int trainEpisode(Trainer* trainer, int episodeNum);

double calculateReward(const GameState* state, const GameState* prevState);
void saveWeights(const QLearningAgent* agent, const char* filename);
void loadWeights(QLearningAgent* agent, const char* filename);

int trainAgainstAlphaBeta(Trainer* trainer, int totalEpisodes);
int trainEpisodeVsAlphaBeta(Trainer* trainer, bool agentPlaysWhite);

double evaluateAgent(QLearningAgent* agent, int totalGames);

#endif // TRAINER_H
