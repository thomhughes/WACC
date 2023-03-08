#include <math.h>
#include <stdlib.h>
#include "wacc.h"

int WACC(pow)(int n, int index) {
    return pow(n, index);
}

int WACC(srand)(unsigned int seed) {
    srand(seed);
    return 0;
}

int WACC(rand)(void) {
    return rand();
}