#ifndef MAGIC_H
#define MAGIC_H
#include "board.h"

const u8 binCoe[] = {1,0,0,0,0,1,1,0,0,0,1,2,1,0,0,1,3,3,1,0,1,4,6,4,1};

const u8 countValSeq[] = {0,0,0,0,0,0,0,1,1,1,1,1,0,1,2,2,2,2,0,1,3,4,4,4,0,1,4,7,8,8,0,1,5,11,15,16};

const u16 dseq[][5] = {
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {1, 0, 0, 0, 0},
    {1, 0, 0, 0, 0},
    {1, 0, 0, 0, 0},
    {1, 0, 0, 0, 0},
    {1, 0, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {2, 0, 0, 0, 0},
    {2, 9, 0, 0, 0},
    {2, 9, 0, 0, 0},
    {2, 9, 0, 0, 0},
    {2, 9, 0, 0, 0},
    {0, 0, 0, 0, 0},
    {3, 0, 0, 0, 0},
    {3, 10, 10, 0, 0},
    {3, 10, 10, 73, 0},
    {3, 10, 10, 73, 0},
    {3, 10, 10, 73, 0},
    {0, 0, 0, 0, 0},
    {4, 0, 0, 0, 0},
    {4, 11, 11, 11, 0},
    {4, 11, 11, 11, 74},
    {4, 11, 11, 11, 74},
    {4, 11, 11, 11, 74},
    {0, 0, 0, 0, 0},
    {5, 0, 0, 0, 0},
    {5, 12, 12, 12, 12},
    {5, 12, 12, 12, 12},
    {5, 12, 12, 12, 12},
    {5, 12, 12, 12, 12}
};

const u16* dseqcrush[] = {
    (const u16[]){1, 0, 0, 0, 0},
    NULL,  // Empty row
    NULL,  // Empty row
    NULL,  // Empty row
    NULL,  // Empty row
    (const u16[]){1, 0, 0, 0, 0},
    (const u16[]){1, 0, 0, 0, 0},
    NULL,  // Empty row
    NULL,  // Empty row
    (const u16[]){1, 0, 0, 0, 0},
    (const u16[]){1, 0, 0, 0, 0},
    (const u16[]){9, 0, 0, 0, 0},
    NULL,  // Empty row
    NULL,  // Empty row
    (const u16[]){1, 0, 0, 0, 0},
    (const u16[]){1, 0, 0, 0, 0},
    (const u16[]){9, 9, 0, 0, 0},
    (const u16[]){72, 0, 0, 0, 0},
    NULL,  // Empty row
    (const u16[]){1, 0, 0, 0, 0},
    (const u16[]){1, 0, 0, 0, 0},
    (const u16[]){10, 10, 10, 0, 0},
    (const u16[]){73, 73, 73, 0, 0},
    (const u16[]){584, 0, 0, 0, 0}
};

// generated with

/* 
   The following explains what we will do for dropSequence and its
   associates helper functions.

https://artofproblemsolving.com/wiki/index.php/Ball-and-urn#Restrictions 

Even though our actual problem is using up to (count) balls 
here we are going to use all (count) balls and iteration through
all possible counts when we are generating moves.

This is a specific case where we want the balls to be left aligned.
If a space is empty all spaces to the right of it must also be empty.

To do this we can look at the restriction of this problem where we have
k = 1 each urn must have at least one ball. If we have up to (spaces) to work
with we can have m spaces and sum for all valid ms 1 <= m <= spaces.
*/


/* #include <stdint.h> */
/* #include <stdbool.h> */
/* #include <stdlib.h> */
/* #include <stdio.h> */
/* #include <string.h> */
/* #include <ctype.h> */
/*  */
/* #define u8 int_fast8_t */
/* #define u16 uint16_t */
/* #define u32 uint32_t */
/*   */
/* u8 binomialCoefficient(u8 n, u8 k) { */
/*     if (k > n) return 0; */
/*     if (k == 0 || k == n) return 1; */
/*  */
/*     if (k > n - k) k = n - k; */
/*  */
/*     u8 result = 1; */
/*     for (u8 i = 0; i < k; i++) { */
/*         result = result * (n - i) / (i + 1); */
/*     } */
/*  */
/*     return result; */
/* } */
/*  */
/* u8 countValidSequences(u8 count, u8 spaces) { */
/*     /* return countValSeq[count * MAX_DROPS + spaces]; */
/*     u8 total = 0; */
/*     if (count == 0 || spaces == 0) return 0; */
/*     for (u8 m = 1; m <= spaces; m++) { */
/*         total += binomialCoefficient(count - 1, m - 1); */
/*     } */
/*     return total; */
/* } */
/*  */
/* u16* dropSequence(u8 count, u8 spaces) { */
/*     u32 total = countValidSequences(count, spaces); */
/*     uint16_t* sequences = malloc(total * sizeof(uint16_t)); */
/*  */
/*     if (!sequences) { */
/*         printf("Memory allocation failed for sequences array\n"); */
/*         return NULL; */
/*     } */
/*  */
/*     u8 index = 0; */
/*     for (u8 spacesFilled = 1; spacesFilled <= spaces; spacesFilled++) { */
/*         u32 numConfigurations = binomialCoefficient(count - 1, spacesFilled - 1); */
/*         if (numConfigurations == 0) continue; */
/*  */
/*         for (u8 configIdx = 0; configIdx < numConfigurations; configIdx++) { */
/*             uint16_t packedDrops = 0; */
/*             u8 remaining = count; */
/*             u8 usedSpaces = 0; */
/*  */
/*             for (u8 i = 0; i < spacesFilled - 1; i++) { */
/*                 u8 dropValue = (remaining > 1) ? (remaining - (spacesFilled - i - 1)) : 1; */
/*                 packedDrops |= (dropValue << (i * 3)); */
/*                 remaining -= dropValue; */
/*                 usedSpaces++; */
/*             } */
/*  */
/*             packedDrops |= (remaining << (usedSpaces * 3));  // Place last value in remaining space */
/*             sequences[index + configIdx] = packedDrops; */
/*         } */
/*  */
/*         index += numConfigurations; */
/*     } */
/*  */
/*     return sequences; */
/* } */
/*  */
/* u16* dropSequencesForCrush(u8 count, u8 spaces) { */
/*     if (count < spaces) { */
/*         printf("Invalid input: count must be greater than or equal to spaces\n"); */
/*         return NULL; */
/*     } */
/*  */
/*     u32 total = binomialCoefficient(count - 1, spaces - 1); */
/*     uint16_t* sequences = malloc(total * sizeof(uint16_t)); */
/*  */
/*     if (!sequences) { */
/*         printf("Memory allocation failed for sequences array\n"); */
/*         return NULL; */
/*     } */
/*  */
/*     for (u8 i = 0; i < total; i++) { */
/*         uint16_t packedDrops = 0; */
/*         u8 remaining = count - 1; */
/*         u8 usedSpaces = 0; */
/*  */
/*         for (u8 j = 0; j < spaces - 1; j++) { */
/*             u8 dropValue = (remaining > 1) ? (remaining - (spaces - j - 1)) : 1; */
/*             packedDrops |= (dropValue << (j * 3)); */
/*             remaining -= dropValue; */
/*             usedSpaces++; */
/*         } */
/*  */
/*         packedDrops |= (1 << (usedSpaces * 3));  // Ensure the last value is always 1 for crush */
/*         sequences[i] = packedDrops; */
/*     } */
/*  */
/*     return sequences; */
/* } */
/*  */
/*  */
/* int main() { */
/*     /* u16* sequences = dropSequence(2, 2); */ 
/*     /* for (u32 i = 0; i < 10; i++) { */ 
/*     /*     printf("%d\n", sequences[i]); */ 
/*     /* } */ 
/*     for (u8 i = 0; i < 6; i++) { */
/*         for (u8 j = 0; j < 6; j++) { */
/*             u16* sequences = dropSequence(i, j); */
/*             if (sequences) { */
/*                 printf("{"); */
/*                 for (u32 k = 0; k < countValidSequences(i, j); k++) { */
/*                     printf("%d,", sequences[k]); */
/*                 } */
/*                 printf("}\n"); */
/*                 free(sequences); */
/*             } */
/*             else { */
/*                 printf("{}\n"); */
/*             } */
/*         } */
/*     } */
/* } */


#endif


