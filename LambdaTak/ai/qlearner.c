#include "qlearner.h"

QLearningAgent* createQLearningAgent(double alpha, double gamma, double epsilon) {
    QLearningAgent* agent = malloc(sizeof(QLearningAgent));
    agent->theta = malloc(STATE_ACTION_FEATURES_SIZE * sizeof(double));
    for (int i = 0; i < STATE_ACTION_FEATURES_SIZE; i++) {
        agent->theta[i] = (rand() / (double)RAND_MAX) * 0.01;
    }
    agent->alpha = alpha;
    agent->gamma = gamma;
    agent->epsilon = epsilon;
    return agent;
}

void freeQLearningAgent(QLearningAgent* agent) {
    free(agent->theta);
    free(agent);
}

double computeQValue(const QLearningAgent* agent, const Features stateFeatures, const double* actionFeatures) {
    double q = 0.0;
    for (int i = 0; i < FEATURES_SIZE; i++) {
        q += stateFeatures[i] * agent->theta[i];
    }
    for (int i = 0; i < ACTION_FEATURES_SIZE; i++) {
        q += actionFeatures[i] * agent->theta[FEATURES_SIZE + i];
    }
    return q;
}

void getActionFeatures(const Move* move, double* features) {
    memset(features, 0, ACTION_FEATURES_SIZE * sizeof(double));
    if (move->type == PLACE) {
        features[0] = 1.0;
        Position pos = move->move.place.pos;
        int x = GET_X(pos);
        int y = GET_Y(pos);
        features[1] = x / (double)(BOARD_SIZE - 1);
        features[2] = y / (double)(BOARD_SIZE - 1);
        switch (move->move.place.stone) {
            case FLAT: features[3] = 1.0; break;
            case STANDING: features[4] = 1.0; break;
            case CAP: features[5] = 1.0; break;
        }
        features[6] = (move->move.place.color == WHITE) ? 1.0 : 0.0;
    } else {
        features[7] = 1.0; 
        Position startPos = move->move.slide.startPos;
        int x = GET_X(startPos);
        int y = GET_Y(startPos);
        features[8] = x / (double)(BOARD_SIZE - 1);
        features[9] = y / (double)(BOARD_SIZE - 1);
        switch (move->move.slide.direction) {
            case LEFT: features[10] = 1.0; break;
            case RIGHT: features[11] = 1.0; break;
            case UP: features[12] = 1.0; break;
            case DOWN: features[13] = 1.0; break;
        }
        features[14] = move->move.slide.count / (double)MAX_PICKUP;
        u16 drops = move->move.slide.drops;
        for (int i = 0; i < 5; i++) {
            int drop = (drops >> (3 * i)) & 0x7;
            features[15 + i] = drop / (double)MAX_PICKUP;
        }
        features[20] = (move->move.slide.crush == CRUSH) ? 1.0 : 0.0;
    }
}

Move selectAction(const QLearningAgent* agent, const GameState* state, const GeneratedMoves* moves, const Features stateFeatures) {
    if ((rand() / (double)RAND_MAX) < agent->epsilon) {
        // Explore: random action
        return moves->moves[rand() % moves->numMoves];
    } else {
        // Exploit: best action based on Q-values
        double maxQ = -INFINITY;
        int bestIdx = 0;
        double actionFeatures[ACTION_FEATURES_SIZE];
        for (u32 i = 0; i < moves->numMoves; i++) {
            getActionFeatures(&moves->moves[i], actionFeatures);
            double q = computeQValue(agent, stateFeatures, actionFeatures);
            if (q > maxQ) {
                maxQ = q;
                bestIdx = i;
            }
        }
        return moves->moves[bestIdx];
    }
}

void updateQLearning(QLearningAgent* agent, const Features prevStateFeatures, const double* actionFeatures, 
                     double reward, const Features nextStateFeatures, const GeneratedMoves* nextMoves) {
    double maxNextQ = -INFINITY;
    double nextActionFeatures[ACTION_FEATURES_SIZE];
    if (nextMoves != NULL) {
        for (u32 i = 0; i < nextMoves->numMoves; i++) {
            getActionFeatures(&nextMoves->moves[i], nextActionFeatures);
            double q = computeQValue(agent, nextStateFeatures, nextActionFeatures);
            if (q > maxNextQ) maxNextQ = q;
        }
    }
    double target = reward + agent->gamma * maxNextQ;
    double currentQ = computeQValue(agent, prevStateFeatures, actionFeatures);
    double delta = target - currentQ;

    for (int i = 0; i < FEATURES_SIZE; i++) {
        agent->theta[i] += agent->alpha * delta * prevStateFeatures[i];
    }
    for (int i = 0; i < ACTION_FEATURES_SIZE; i++) {
        agent->theta[FEATURES_SIZE + i] += agent->alpha * delta * actionFeatures[i];
    }
}
