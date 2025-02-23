#ifndef MAG
#define MAGIC_H
#include "board.h"

#define DROP_SEQUENCE(count, spaces) dseq[((count)-1)*6+(spaces)-1]
// doesn't need -1 because crush square isn't counted when called
#define DROP_SEQUENCE_CRUSH(count, spaces) dseqcrush[((count)-1)*6+(spaces)]
#define BIN_COEF(n, k) binCoe[(n)*MAX_DROPS+(k)]
#define COUNT_VAL_SEQ(count, spaces) countValSeq[(count)*7+(spaces)]

const u8 binCoe[] = {1,0,0,0,0,1,1,0,0,0,1,2,1,0,0,1,3,3,1,0,1,4,6,4,1};

const u8 countValSeq[] = {0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,2,2,2,2,2,0,1,3,4,4,4,4,0,1,4,7,8,8,8,0,1,5,11,15,16,16,0,1,6,16,26,31,32};

const u16* dseq[] = {
    (const u16[]){1, 0},
    (const u16[]){1, 0},
    (const u16[]){1, 0},
    (const u16[]){1, 0},
    (const u16[]){1, 0},
    (const u16[]){1, 0},
    (const u16[]){2, 0},
    (const u16[]){2, 9, 0},
    (const u16[]){2, 9, 0},
    (const u16[]){2, 9, 0},
    (const u16[]){2, 9, 0},
    (const u16[]){2, 9, 0},
    (const u16[]){3, 0},
    (const u16[]){3, 17, 10, 0},
    (const u16[]){3, 17, 10, 73, 0},
    (const u16[]){3, 17, 10, 73, 0},
    (const u16[]){3, 17, 10, 73, 0},
    (const u16[]){3, 17, 10, 73, 0},
    (const u16[]){4, 0},
    (const u16[]){4, 25, 18, 11, 0},
    (const u16[]){4, 25, 18, 11, 137, 81, 74, 0},
    (const u16[]){4, 25, 18, 11, 137, 81, 74, 585, 0},
    (const u16[]){4, 25, 18, 11, 137, 81, 74, 585, 0},
    (const u16[]){4, 25, 18, 11, 137, 81, 74, 585, 0},
    (const u16[]){5, 0},
    (const u16[]){5, 33, 26, 19, 12, 0},
    (const u16[]){5, 33, 26, 19, 12, 201, 145, 89, 138, 82, 75, 0},
    (const u16[]){5, 33, 26, 19, 12, 201, 145, 89, 138, 82, 75, 1097, 649, 593, 586, 0},
    (const u16[]){5, 33, 26, 19, 12, 201, 145, 89, 138, 82, 75, 1097, 649, 593, 586, 4681, 0},
    (const u16[]){5, 33, 26, 19, 12, 201, 145, 89, 138, 82, 75, 1097, 649, 593, 586, 4681, 0},
    (const u16[]){6, 0},
    (const u16[]){6, 41, 34, 27, 20, 13, 0},
    (const u16[]){6, 41, 34, 27, 20, 13, 265, 209, 153, 97, 202, 146, 90, 139, 83, 76, 0},
    (const u16[]){6, 41, 34, 27, 20, 13, 265, 209, 153, 97, 202, 146, 90, 139, 83, 76, 1609, 1161, 713, 1105, 657, 601, 1098, 650, 594, 587, 0},
    (const u16[]){6, 41, 34, 27, 20, 13, 265, 209, 153, 97, 202, 146, 90, 139, 83, 76, 1609, 1161, 713, 1105, 657, 601, 1098, 650, 594, 587, 8777, 5193, 4745, 4689, 4682, 0},
    (const u16[]){6, 41, 34, 27, 20, 13, 265, 209, 153, 97, 202, 146, 90, 139, 83, 76, 1609, 1161, 713, 1105, 657, 601, 1098, 650, 594, 587, 8777, 5193, 4745, 4689, 4682, 37449, 0},
};

const u16* dseqcrush[] = {
    //Distributions of 1 balls into 1 urns:
    (const u16[]){1, 0}, // 0,0 - 0
    //No distributions of 1 balls into 2 urns.
    (const u16[]){0}, //0,1 - 1
    //No distributions of 1 balls into 3 urns.
    (const u16[]){0}, //0,2 - 2
    //No distributions of 1 balls into 4 urns.
    (const u16[]){0}, //0,3 - 3
    //No distributions of 1 balls into 5 urns.
    (const u16[]){0}, //0,4 - 4
    //No distributions of 1 balls into 6 urns.
    (const u16[]){0}, //0,5 - 5
    //No distributions of 2 balls into 1 urns.
    (const u16[]){0}, //1,0 - 6
    //Distributions of 2 balls into 2 urns:
    (const u16[]){9, 0}, //1, 1 - 7
    //No distributions of 2 balls into 3 urns.
    (const u16[]){0}, //8
    //No distributions of 2 balls into 4 urns.
    (const u16[]){0}, //9
    //No distributions of 2 balls into 5 urns.
    (const u16[]){0}, //10
    //No distributions of 2 balls into 6 urns.
    (const u16[]){0}, //11
    //No distributions of 3 balls into 1 urns.
    (const u16[]){0} , //12
    //Distributions of 3 balls into 2 urns:
    (const u16[]){10, 0}, //13
    //Distributions of 3 balls into 3 urns:
    (const u16[]){73, 0}, //14
    //No distributions of 3 balls into 4 urns.
    (const u16[]){0},
    //No distributions of 3 balls into 5 urns.
    (const u16[]){0},
    //No distributions of 3 balls into 6 urns.
    (const u16[]){0},
    //No distributions of 4 balls into 1 urns.
    (const u16[]){0},
    //Distributions of 4 balls into 2 urns:
    (const u16[]){11, 0},
    //Distributions of 4 balls into 3 urns:
    (const u16[]){81, 74, 0},
    //Distributions of 4 balls into 4 urns:
    (const u16[]){585, 0},
    //No distributions of 4 balls into 5 urns.
    (const u16[]){0},
    //No distributions of 4 balls into 6 urns.
    (const u16[]){0},
    //No distributions of 5 balls into 1 urns.
    (const u16[]){0},
    //Distributions of 5 balls into 2 urns:
    (const u16[]){12, 0},
    //Distributions of 5 balls into 3 urns:
    (const u16[]){89, 82, 75, 0},
    //Distributions of 5 balls into 4 urns:
    (const u16[]){649, 593, 586, 0},
    //Distributions of 5 balls into 5 urns:
    (const u16[]){4681, 0},
    //No distributions of 5 balls into 6 urns.
    (const u16[]){0},
    //No distributions of 6 balls into 1 urns.
    (const u16[]){0},
    //Distributions of 6 balls into 2 urns:
    (const u16[]){13, 0},
    //Distributions of 6 balls into 3 urns:
    (const u16[]){97, 90, 83, 76, 0},
    //Distributions of 6 balls into 4 urns:
    (const u16[]){713, 657, 601, 650, 594, 587, 0},
    //Distributions of 6 balls into 5 urns:
    (const u16[]){5193, 4745, 4689, 4682, 0},
    //Distributions of 6 balls into 6 urns:
    (const u16[]){37449, 0},
};

#endif
