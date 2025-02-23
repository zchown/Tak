// Generates Magic.h
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef uint16_t u16;
typedef uint8_t u8;
typedef size_t usize;

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
