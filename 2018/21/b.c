#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

long f(long);

int main()
{
    for (long val = 0;; val = f(val | 0x10000));
}

long f(long valB) {
    long valA = 10736359;
    long lastValB;

    do {
        valA += valB & 255;
        valA &= 16777215;
        valA *= 65899;
        valA &= 16777215; // last value produced in this range is our target

        lastValB = valB;
        valB >>= 8;
    } while (lastValB >= 256);

    printf("%ld\n", valA);

    return valA;
}

