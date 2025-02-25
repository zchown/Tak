// Generates Magic.h
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef uint16_t u16;
typedef uint8_t u8;
typedef size_t usize;
typedef uint64_t Bitboard;

#define BOARD_SIZE 6

typedef struct {
    u16 *data;
    usize size;
    usize capacity;
} ResultArray;

void addResult(ResultArray *res, u16 value) {
    if (res->size >= res->capacity) {
        usize newCapacity = (res->capacity == 0) ? 16 : res->capacity * 2;
        u16 *newData = realloc(res->data, newCapacity * sizeof(u16));
        if (!newData) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(1);
        }
        res->data = newData;
        res->capacity = newCapacity;
    }
    res->data[res->size++] = value;
}

void generateDistributions(u8 used, u8 index, u8 *current,
                         u8 remaining, ResultArray *res, u8 total) {
    if (index == used - 1) {
        current[index] = remaining;
        u16 packed = 0;
        for (u8 i = 0; i < used; i++) {
            packed |= (current[i] & 0x07) << (3 * i);
        }
        addResult(res, packed);
        return;
    }
    for (u8 i = 1; i <= remaining - (used - index - 1); i++) {
        current[index] = i;
        generateDistributions(used, index + 1, current, remaining - i, res, total);
    }
}

u16* distributeBalls(u8 balls, u8 total, usize *resultCount) {
    ResultArray res = {NULL, 0, 0};
    u8 current[total];
    for (u8 used = 1; used <= total; used++) {
        if (balls < used) continue;
        generateDistributions(used, 0, current, balls, &res, total);
    }
    *resultCount = res.size;
    return res.data;
}

void generateDistributionsSpecial(u8 remaining, u8 urns, u8 index, u8 *current, ResultArray *res) {
    if (index == urns - 1) {
        current[index] = remaining;
        u16 packed = 0;
        for (u8 i = 0; i < urns; i++) {
            packed |= (current[i] & 0x07) << (3 * i);
        }
        packed |= (1 & 0x07) << (3 * urns);
        addResult(res, packed);
        return;
    }
    for (u8 i = 1; i <= remaining - (urns - index - 1); i++) {
        current[index] = i;
        generateDistributionsSpecial(remaining - i, urns, index + 1, current, res);
    }
}

u16* distributeBallsSpecial(u8 balls, u8 urns, usize *resultCount) {
    if (urns < 1) {
        fprintf(stderr, "Invalid number of urns.\n");
        *resultCount = 0;
        return NULL;
    }
    if (urns == 1) {
        if (balls != 1) {
            fprintf(stderr, "Invalid input: With 1 urn, only 1 ball is allowed under special condition.\n");
            *resultCount = 0;
            return NULL;
        }
        u16 *result = malloc(sizeof(u16));
        if (!result) {
            fprintf(stderr, "Memory allocation failed.\n");
            *resultCount = 0;
            return NULL;
        }
        result[0] = 1;
        *resultCount = 1;
        return result;
    }

    if (balls < urns) {
        fprintf(stderr, "Invalid input: Each urn must have at least one ball.\n");
        *resultCount = 0;
        return NULL;
    }
    u8 firstUrns = urns - 1;
    u8 remainingBalls = balls - 1;

    u8 *current = malloc(firstUrns * sizeof(u8));
    if (!current) {
        fprintf(stderr, "Memory allocation failed.\n");
        *resultCount = 0;
        return NULL;
    }

    ResultArray res = {NULL, 0, 0};
    generateDistributionsSpecial(remainingBalls, firstUrns, 0, current, &res);
    free(current);

    *resultCount = res.size;
    return res.data;
}

void printDistribution(u16 packed, u8 urns) {
    printf("[");
    for (u8 i = 0; i < urns; i++) {
        u8 val = (packed >> (3 * i)) & 0x07;
        printf("%d%s", val, (i == urns - 1) ? "" : ", ");
    }
    printf("]");
}


void generateBitBoards(void) {
    Bitboard rowMasks[BOARD_SIZE];
    Bitboard colMasks[BOARD_SIZE];

    for (unsigned row = 0; row < BOARD_SIZE; row++) {
        Bitboard mask = 0;
        for (unsigned col = 0; col < BOARD_SIZE; col++) {
            // Set the bit corresponding to (row, col)
            mask |= (1ULL << (row * BOARD_SIZE + col));
        }
        rowMasks[row] = mask;
    }

    for (unsigned col = 0; col < BOARD_SIZE; col++) {
        Bitboard mask = 0;
        for (unsigned row = 0; row < BOARD_SIZE; row++) {
            mask |= (1ULL << (row * BOARD_SIZE + col));
        }
        colMasks[col] = mask;
    }

    printf("Row masks:\n");
    for (unsigned row = 0; row < BOARD_SIZE; row++) {
        printf("Row %u: 0x%016llX\n", row, rowMasks[row]);
    }

    printf("Column masks:\n");
    for (unsigned col = 0; col < BOARD_SIZE; col++) {
        printf("Col %u: 0x%016llX\n", col, colMasks[col]);
    }
}

int main() {
    for (u8 balls = 1; balls <= 6; balls++) {
        for (u8 urns = 1; urns <= 6; urns++) {
            usize resultCount;
            u16 *results = distributeBallsSpecial(balls, urns, &resultCount);
            if (results) {
                printf("//Distributions of %d balls into %d urns:\n", balls, urns);
                printf("(const u16[]){");
                for (usize i = 0; i < resultCount; i++) {
                    printf("%d, ", results[i]);
                }
                free(results);
                printf("0},\n");
            }
            else {
                printf("//No distributions of %d balls into %d urns.\n", balls, urns);
                printf("(const u16[]){0},\n");
            }
        }
    }
    return 0;
}
