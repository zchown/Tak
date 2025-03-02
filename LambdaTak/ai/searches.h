# ifndef SEARCHES_H
# define SEARCHES_H
#include "../lib/board.h"
#include "../lib/moves.h"
#include "eval.h"
#include <time.h>
#include <math.h>

#define MAX_DEPTH 64
#define KILLER_MOVES 2
#include <math.h>

extern Move killerMoves[MAX_DEPTH][KILLER_MOVES];
extern int historyHeuristic[NUM_COLORS][TOTAL_SQUARES][TOTAL_SQUARES];
static const u32 TRANSPOSITION_TABLE_SIZE = (1 << 29);

typedef enum {UNDER, OVER, EXACT} EstimationType;

typedef struct {
    Move move;
    int score;
    int depth;
    EstimationType type;
    ZobristKey hash;
} TranspositionEntry;

extern TranspositionEntry* transpositionTable;

typedef struct SearchStatistics {
    int maxDepth;
    int totalNodes;
    u64 generatedMoves;
    int timeLimit;
    int transpositionHits;
    int transpositionMisses;
    int transpositionDepthRewrites;
    int transpositionCollisions;
    int transpositionCutOffs;
    int transpositionFill;
    int alphaBetaCutoffs;
    int failHighResearches;
    int transpositionTableUpdates;
    int transpositionLookups;
    int reducedDepthSearches;
} SearchStatistics;

Move iterativeDeepeningSearch(GameState* state, int timeLimit);

Move negaMaxRoot(GameState* state, int depth, bool* timeUp, double startTime, int timeLimit, SearchStatistics* stats);

int negaMax(GameState* state, int depth, int alpha, int beta, int color, bool* timeUp, double startTime, int timeLimit, u32 prevMoves, bool doReducedDepth, SearchStatistics* stats);

static double getTimeMs();

u32 zobristToIndex(ZobristKey hash);
const TranspositionEntry* lookupTranspositionTable(ZobristKey hash, int depth, int alpha, int beta, SearchStatistics* stats);
void updateTranspositionTable(ZobristKey hash, int score, EstimationType type, Move move, int depth, SearchStatistics* stats);

int scoreMove(const GameState* state, const Move* move, const Move* bestMove);
int compareMoves(const GameState* state, const Move* a, const Move* b, const Move* bestMove);

void sortMoves(GameState* state, Move* moves, int numMoves);
void clearKillerMoves(void);
void clearHistoryHeuristic(void);
void clearTranspositionTable(void);

void printSearchStats(const SearchStatistics* stats);

typedef struct MCTSNode {
    int numVisits;
    Color toPlay;
    Color originalToPlay;
    double prior; // Prior probability of selecting this node
    struct MCTSNode* parent;
    struct MCTSNode** children;
    u32 numChildren;
    double valueSum;
    Move move;
} MCTSNode;

#define DEFAULT_UCT_CONSTANT 1.41421356237  // sqrt(2)
#define MAX_MCTS_ITERATIONS 100000
#define MIN_PLAYOUTS_PER_NODE 5
#define MAX_TURNS 250

#define MCTSNODE_VALUE(node) ((node)->valueSum / (double)(node)->numVisits)
#define MCTSNODE_EXPANDED(node) ((node)->numChildren > 0)

Move monteCarloTreeSearch(GameState* state, int timeLimit);

MCTSNode* selectNode(MCTSNode* node, GameState* state);
MCTSNode* expand(MCTSNode* node, GameState* state, double prior);
void backup(MCTSNode* node, double value, int toPlay);
double simulate(GameState* state);

double ucbScore(MCTSNode* parent, MCTSNode* child);

MCTSNode* createMCTSNode(Color toPlay, MCTSNode* parent, double prior, Move move);
void freeMCTSNode(MCTSNode* node);

#endif // SEARCHES_H
