#include "policyNetwork.h"

void addToSearchProb(SearchProb* prob, Move move, float weight) {
    prob->count += 1.0;
    if (move.type == PLACE) {
        prob->place += weight;
        prob->squares[move.move.place.pos] += weight;
        prob->pieceType[move.move.place.stone] += weight;
    } else if (move.type == SLIDE) {
        prob->slide += weight;
        prob->squares[move.move.slide.startPos] += weight;
        prob->drops[(move.move.slide.count)-1][move.move.slide.direction] += weight;
    }
}

void toOutput(const SearchProb* prob, double value, double* output) {
    output[0] = value;
    output[1] = (double) prob->place;
    output[2] = (double) prob->slide;

    output[3] = (double) prob->pieceType[FLAT];
    output[4] = (double) prob->pieceType[CAP];
    output[5] = (double) prob->pieceType[STANDING];

    for (int i = 0; i < TOTAL_SQUARES; i++) {
        output[3 + NUM_PIECE_TYPES + i] = (double) prob->squares[i];
    }

    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 4; j++) {
            output[3 + NUM_PIECE_TYPES + TOTAL_SQUARES + i * 5 + j] = (double) prob->drops[i][j];
        }
    }
}

double probFromSearchProb(const SearchProb* prob, const Move* move) {
    if (move->type == PLACE) {
        double squareProb = prob->squares[move->move.place.pos] / prob->place;
        double pieceProb = prob->pieceType[move->move.place.stone] / prob->place;
        return squareProb * pieceProb;
    } else if (move->type == SLIDE) {
        /* printf("direction: %d\n", move->move.slide.direction); */
        /* printf("count: %d\n", move->move.slide.count); */
        double squareProb = prob->squares[move->move.slide.startPos] / prob->slide;
        double dropProb = prob->drops[(move->move.slide.count) - 1][move->move.slide.direction] / prob->slide;
        /* printf("squareProb: %f, dropProb: %f\n", squareProb, dropProb); */
        return squareProb * dropProb;
    }
    return 0.0;
}

void probsFromSearchProb(const SearchProb* prob, const GeneratedMoves* moves, double* probs) {
    double total = 0.0;
    for (int i = 0; i < moves->numMoves; i++) {
        probs[i] = probFromSearchProb(prob, &moves->moves[i]);
        total += probs[i];
    }

    for (int i = 0; i < moves->numMoves; i++) {
        probs[i] /= total;
    }
}

SearchProb outputToSearchProb(const double* probs) {
    SearchProb prob = {0};
    prob.count = 1.0;
    prob.place = probs[1];
    prob.slide = probs[2];

    prob.pieceType[FLAT] = probs[3];
    prob.pieceType[CAP] = probs[4];
    prob.pieceType[STANDING] = probs[5];

    for (int i = 0; i < TOTAL_SQUARES; i++) {
        prob.squares[i] = probs[2 + NUM_PIECE_TYPES + i];
    }

    for (int i = 0; i < MAX_DROPS; i++) {
        for (int j = 0; j < 4; j++) {
            prob.drops[i][j] = probs[5 + TOTAL_SQUARES + i * 4 + j];
        }
    }
    return prob;
}
