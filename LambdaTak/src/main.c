#include "perft.h"
#include "tps.h"

int main() {
    char* tps = "[TPS 2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/1S,1S,1S,1S,1S,1S/2S,2S,2S,2S,2S,2S/11,x5 1 3]";
    GameState* state = parseTPS(tps);

    /* u8** drops = dropSequencesForCrush(1, 0); */
    /* u32 total = binomialCoefficient(0, -1); */
    /* printf("Total: %d\n", total); */
    /* for (u8 i = 0; i < 6; i++) { */
    /*     printf("%d: ", drops[i][0]); */
    /* } */
    /* GameState* state = createGameState(); */
    runPerft(state, 6);
    return 0;
}

