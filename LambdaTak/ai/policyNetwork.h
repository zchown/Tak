#ifndef POLICYNETWORK_H
#define POLICYNETWORK_H

#include "../lib/board.h"
#include "../lib/moves.h"

typedef struct {
    float count;
    float place;
    float slide;
    float pieceType [NUM_PIECE_TYPES];
    float squares[TOTAL_SQUARES];
    float drops [6][4]; // count, direction
} SearchProb;

void addToSearchProb(SearchProb* prob, Move move, float weight);

void toOutput(const SearchProb* prob, double value, double* output);

double probFromSearchProb(const SearchProb* prob, const Move* move);

void probsFromSearchProb(const SearchProb* prob, const GeneratedMoves* moves, double* probs);

SearchProb outputToSearchProb(const double* output);

#endif // POLICYNETWORK_H
