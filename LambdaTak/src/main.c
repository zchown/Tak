#include "aiPlayer.h"
#include "perft.h"
#include "tps.h"

int main() {
    /* return runAI(); */
    GameState* state = createGameState();
    /* GameState* state = parseTPS("[TPS 2,2,21S,2,2,2/2,x,222221,2,2,x/1,1,2221C,x,111112C,2S/x,1,2S,x2,121211212/1,1,1212S,1S,2,1S/x2,2,1,21,1 1 42]"); */

    /* GameState* state = parseTPS("[TPS 2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/11,x5 1 3]"); */
    runPerft(state, 7);
    freeGameState(state);
    return 0;
}

