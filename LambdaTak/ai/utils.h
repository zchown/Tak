#ifndef UTILS_H
#define UTILS_H

#include "../lib/board.h"
#include <math.h>
#include "eval.h"

#define FEATURES_SIZE ((TOTAL_SQUARES * 4) + 5)

typedef int* Features;

/* turn, num white flats, num black flats
    num white controlled, num black controlled 
    for each square:
        piece type, num pieces, num white stones, num black stones */
Features getFeatures(GameState* state, Features features);

#define SIGMOID(x) (1 / (1 + exp(-x)))
#define RELU(x) (x > 0 ? x : 0)
#define TANH(x) ((exp(x) - exp(-x)) / (exp(x) + exp(-x)))

#define MEAN_SQUARED_ERROR(x, y) ((x - y) * (x - y))
#define CROSS_ENTROPY(x, y) (-y * log(x) - (1 - y) * log(1 - x))
#define SOFTMAX(x, y) (exp(x) / y)

// total number of possible tak moves
// 201,204
// so instead we can do hierarchical action generation
// decide on type of move, then decide on position, then decide on direction
// then decide on count, then decide on drops

#endif // UTILS_H
