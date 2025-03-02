#ifndef Q_LEARNING_H
#define Q_LEARNING_H

#include "utils.h"
#include "../lib/moves.h"
#include "../lib/board.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define ACTION_FEATURES_SIZE 21
#define STATE_ACTION_FEATURES_SIZE (FEATURES_SIZE + ACTION_FEATURES_SIZE)

typedef struct {
    double* theta;  // Weight vector for Q-value approximation
    double alpha;   // Learning rate
    double gamma;   // Discount factor
    double epsilon; // Exploration rate
} QLearningAgent;

QLearningAgent* createQLearningAgent(double alpha, double gamma, double epsilon);

void freeQLearningAgent(QLearningAgent* agent);

double computeQValue(const QLearningAgent* agent, const Features stateFeatures, const double* actionFeatures);

void getActionFeatures(const Move* move, double* features);

Move selectAction(const QLearningAgent* agent, const GameState* state, const GeneratedMoves* moves, const Features stateFeatures);

void updateQLearning(QLearningAgent* agent, const Features prevStateFeatures, const double* actionFeatures, 
                     double reward, const Features nextStateFeatures, const GeneratedMoves* nextMoves);

#endif // Q_LEARNING_H
