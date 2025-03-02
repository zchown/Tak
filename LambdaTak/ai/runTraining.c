#include "trainer.h"
#include "qlearner.h"
#include "../lib/board.h"
#include "utils.h"

int main() {
    srand(time(NULL));
    initZobristTable();

    QLearningAgent* agent = createQLearningAgent(0.1, 0.9, 0.1);
    Trainer* trainer = createTrainer(agent, 0.999, 0.01, 1000);

    train(trainer, MAX_EPISODES);

    trainAgainstAlphaBeta(trainer, MAX_EPISODES);

    evaluateAgent(agent, 1000);

    train(trainer, MAX_EPISODES);

    trainAgainstAlphaBeta(trainer, MAX_EPISODES);

    train(trainer, MAX_EPISODES);

    evaluateAgent(agent, 1000);

    freeTrainer(trainer);
    freeQLearningAgent(agent);

    return 0;
}
