#include "moves.h"

MoveResult checkMove(GameState* state, Move* move);

GameState* makeMoveChecks(GameState* state, Move* move);
GameState* makeMoveNoChecks(GameState* state, Move* move);

GameState* undoMoveChecks(GameState* state);
GameState* undoMoveNoChecks(GameState* state);

Move* generateAllMoves(GameState* state);



/* 
The following explains what we will do for dropSequence and its
associates helper functions.

https://artofproblemsolving.com/wiki/index.php/Ball-and-urn#Restrictions 

Even though our actual problem is using up to (count) balls 
here we are going to use all (count) balls and iteration through
all possible counts when we are generating moves.

This is a specific case where we want the balls to be left aligned.
If a space is empty all spaces to the right of it must also be empty.

To do this we can look at the restriction of this problem where we have
k = 1 each urn must have at least one ball. If we have up to (spaces) to work
with we can have m spaces and sum for all valid ms 1 <= m <= spaces.
*/
u32** dropSequence(u32 count, u32 spaces) {
    // Calculate how many valid configurations exist
    u32 total = countValidSequences(count, spaces);

    // Allocate memory for pointers to each configuration
    u32** sequences = malloc(total * sizeof(u32*));
    if (!sequences) {
        printf("Memory allocation failed for sequences array\n");
        return NULL;
    }

    // Allocate memory for each individual configuration
    for (u32 i = 0; i < total; i++) {
        sequences[i] = malloc(spaces * sizeof(u32));
        if (!sequences[i]) {
            printf("Memory allocation failed for sequence %u\n", i);
            // Clean up already allocated memory
            for (u32 j = 0; j < i; j++) {
                free(sequences[j]);
            }
            free(sequences);
            return NULL;
        }
    }

    // Generate all valid configurations
    u32 index = 0;
    for (u32 spacesFilled = 1; spacesFilled <= spaces; spacesFilled++) {
        // Calculate how many ways we can choose which spaces to fill
        u32 numConfigurations = binomialCoefficient(count - 1, spacesFilled - 1);
        if (numConfigurations == 0) continue;

        // Generate each possible configuration for this number of filled spaces
        for (u32 configIdx = 0; configIdx < numConfigurations; configIdx++) {
            u32* currentConfig = sequences[index + configIdx];

            // Special case: all balls in first container
            if (spacesFilled == 1) {
                currentConfig[0] = count;
            } 
            else {
                u32 remaining = configIdx;
                u32 n = count - 1;

                for (u32 i = 0; i < spacesFilled - 1; i++) {
                    u32 current = (i == 0) ? 0 : currentConfig[i - 1] + 1;

                    while (1) {
                        u32 combinations = binomialCoefficient(n - current - 1, spacesFilled - i - 2);
                        if (remaining < combinations) break;
                        remaining -= combinations;
                        current++;
                    }

                    currentConfig[i] = current;
                    n = n - current - 1;
                }

                u32 lastIndex = currentConfig[spacesFilled - 2];

                currentConfig[spacesFilled - 1] = (count - 1) - lastIndex;

                for (u32 i = spacesFilled - 2; i > 0; i--) {
                    currentConfig[i] = currentConfig[i] - currentConfig[i - 1];
                }

                // Handle the first position separately
                currentConfig[0] = currentConfig[0] + 1;
            }

            // Fill remaining containers with zeros
            for (u32 i = spacesFilled; i < spaces; i++) {
                currentConfig[i] = 0;
            }
        }

        index += numConfigurations;
    }

    return sequences;
}

/*
This is an easier function as we just have to generate all possible
combinations of putting (count) balls into (spaces) where we have to
use all spaces. And for the crush the last space must be a 1.
*/
u32** dropSequencesForCrush(u32 count, u32 spaces) {
    // this should be checked when calling this function
    // but we will check it here as well
    if (count < spaces) {
        printf("Invalid input: count must be greater than or equal to spaces\n");
        return NULL;
    }

    u32 total = binomialCoefficient(count - 1, spaces - 1);

    u32** sequences = malloc(total * sizeof(u32*));
    if (!sequences) {
        printf("Memory allocation failed for sequences array\n");
        return NULL;
    }

    // +1 for the crush
    for (u32 i = 0; i < total; i++) {
        sequences[i] = malloc((spaces + 1) * sizeof(u32));
        if (!sequences[i]) {
            printf("Memory allocation failed for sequence %u\n", i);
            for (u32 j = 0; j < i; j++) {
                free(sequences[j]);
            }
            free(sequences);
            return NULL;
        }
    }

    u32 index = 0;
    u32 remainingBalls = count - spaces;

    for (u32 i = 0; i < total; i++) {
        for (u32 j = 0; j < spaces; j++) {
            sequences[i][j] = 1;
        }
        sequences[i][spaces] = 1;
    }

    for (u32 i = 0; i < total; i++) {
        if (i > 0) { // Skip the first one, which is already set to 1 for each space
            u32 remaining = i;
            u32 balls = remainingBalls;
            u32 pos = spaces - 1;

            while (balls > 0 && pos > 0) {
                u32 maxCombinations = binomialCoefficient(balls + pos - 1, pos - 1);
                u32 useBalls = 0;

                while (remaining >= maxCombinations) {
                    remaining -= maxCombinations;
                    useBalls++;
                    if (balls > useBalls) {
                        maxCombinations = maxCombinations * (balls - useBalls) / (balls + pos - useBalls);
                    }
                    else {
                        break;
                    }
                }

                sequences[i][pos - 1] += useBalls;
                balls -= useBalls;
                pos--;
            }

            if (balls > 0) {
                sequences[i][0] += balls;
            }
        }
    }

    return sequences;
}

u32 binomialCoefficient(u32 n, u32 k) {
    if (k > n) return 0;
    if (k == 0 || k == n) return 1;

    if (k > n - k) k = n - k;

    u32 result = 1;
    for (u32 i = 0; i < k; i++) {
        result = result * (n - i) / (i + 1);
    }

    return result;
}

u32 countValidSequences(u32 count, u32 spaces) {
    u32 total = 0;
    for (u32 m = 1; m <= spaces; m++) {
        total += binomialCoefficient(count - 1, m - 1);
    }
    return total;
}


u8 numSteps(GameState* state, Position pos, Direction dir) {
    u8 steps = 0;
    Position nextPos = nextPosition(pos, dir);
    while (isValidPosition(nextPos)) {
        Square* sq = readSquare(state->board, nextPos);
        if (sq->head == NULL) {
            steps++;
            nextPos = nextPosition(nextPos, dir);
        } 
        else if (sq->head->stone != FLAT) {
            break;
        }
        else {
            steps++;
            nextPos = nextPosition(nextPos, dir);
        }
    }
    return steps;
}
