#include "utils.h"

Features getFeatures(GameState* state, Features features) {
    int curFeature = 0;
    features[curFeature++] = state->turn;
    features[curFeature++] = __builtin_popcountll(state->whiteControlled ^ 
            state->standingStones ^ state->capstones);
    features[curFeature++] = __builtin_popcountll(state->blackControlled ^ 
            state->standingStones ^ state->capstones);
    features[curFeature++] = __builtin_popcountll(state->whiteControlled);
    features[curFeature++] = __builtin_popcountll(state->blackControlled);
    for (int i = 0; i < TOTAL_SQUARES; i++) {
        Square square = state->board->squares[i];
        if (square.numPieces == 0) {
            features[curFeature++] = 0;
        } else {
            int curPiece = 0;
            if (square.pieces[square.numPieces - 1].stone == FLAT) {
                curPiece = 1;
            } else if (square.pieces[square.numPieces - 1].stone == STANDING) {
                curPiece = 2;
            } else {
                curPiece = 3;
            }

            if (square.pieces[square.numPieces - 1].color != WHITE) {
                features[curFeature++] = curPiece;
            } else {
                features[curFeature++] = -curPiece;
            }
        }

        features[curFeature++] = square.numPieces;
        features[curFeature++] = square.whiteStones;
        features[curFeature++] = square.blackStones;
    }
    return features;
}

