#ifndef MONTECARLOGRAPHSEARCH_H
#define MONTECARLOGRAPHSEARCH_H

// Monte Carlo Graph Search
// https://arxiv.org/pdf/2012.11045
// https://proceedings.mlr.press/v129/leurent20a/leurent20a.pdf
// https://github.com/lightvector/KataGo/blob/master/docs/GraphSearch.md

#include "../lib/board.h"
#include "../lib/moves.h"

#define PUCT_CONSTANT 1.0
#define MONTECARLO_TABLE_SIZE (1 << 26)

typedef struct MCGSNode {
    struct MCGSNode* parent;
    struct MCGSNode** children;
    int numChildren;
    int numVisits;
    double valueSum;
    bool expand;
} MCGSNode;

#endif // MONTECARLOGRAPHSEARCH_H
