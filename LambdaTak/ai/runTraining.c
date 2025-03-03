#include "trainer.h"
#include "qlearner.h"
#include "../lib/board.h"

int main() {
    srand(time(NULL));
    initZobristTable();

    double alphaLearningRate = 0.01;
    double normalLearningRate = 0.0005;

    for (int i = 1; i <= 1000; i++) {
        printf("Iteration %d\n", i);

        alphaLearningRate = alphaLearningRate * (1.0 - (0.005 * i));
        normalLearningRate = normalLearningRate * (1.0 - (0.005 * i));

        QLearningAgent* agent = createQLearningAgent(0.0001, 0.8, 0.2);

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

        if (i % 20 == 0) {
            evaluateAgent(agent, 50);
        }

        freeTrainer(trainer);
        freeQLearningAgent(agent);
    }

    return 0;
}
