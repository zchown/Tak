#include "perft.h"
#include "tps.h"

int main() {
    /* char* tps = "[TPS 2,2,21S,2,2,2/2,x,222221,2,2,x/1,1,2221C,x,111112C,2S/x,1,2S,x2,121211212/1,1,1212S,1S,2,1S/x2,2,1,21,1 1 42]"; */
    /* GameState* state = parseTPS(tps); */

    /* u8** drops = dropSequencesForCrush(1, 0); */
    /* u32 total = binomialCoefficient(0, -1); */
    /* printf("Total: %d\n", total); */
    /* for (u8 i = 0; i < 6; i++) { */
    /*     printf("%d: ", drops[i][0]); */
    /* } */
    GameState* state = createGameState();
    runPerft(state, 6);
    return 0;
}

