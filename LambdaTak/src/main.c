#include "perft.h"
#include "tps.h"

int main() {
    char* tps = "2,x,2,x,22121,x/x4,1,x/x3,22121,x2/x,1,1221,1C,2,x/1,2,x,2C,2,x/1,x5 1 21";
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

