#include "trainer.h"
#include "qlearner.h"
#include "../lib/board.h"

int main() {
    srand(time(NULL));
    initZobristTable();

    double alphaLearningRate = 0.2;
    double normalLearningRate = 0.05;

    for (int i = 0; i < 100; i++) {
        printf("Iteration %d\n", i);

        alphaLearningRate = alphaLearningRate * (1.0 - (0.005 * i));
        normalLearningRate = normalLearningRate * (1.0 - (0.005 * i));

        QLearningAgent* agent = createQLearningAgent(0.1, 0.9, 0.2);

        loadWeights(agent, "model.weights");

        Trainer* trainer = createTrainer(agent, 0.999, 0.01, 1000);

        saveWeights(agent, "model.weights");

        agent->alpha = alphaLearningRate;

        trainAgainstAlphaBeta(trainer, MAX_EPISODES, 50);

        saveWeights(agent, "model.weights");

        agent->alpha = normalLearningRate;

        train(trainer, MAX_EPISODES * 100);

        saveWeights(agent, "model.weights");

        agent->alpha = alphaLearningRate;

        trainAgainstAlphaBeta(trainer, MAX_EPISODES, 100);

        saveWeights(agent, "model.weights");

        agent->alpha = normalLearningRate;

        train(trainer, MAX_EPISODES * 100);

        saveWeights(agent, "model.weights");

        evaluateAgent(agent, 100);

        freeTrainer(trainer);
        freeQLearningAgent(agent);
    }

    return 0;
}
