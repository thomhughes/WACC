#include <string.h>
#include <stdlib.h>
#include "wacc.h"

int WACC(strlen, 0)(char *str)
{
    return strlen(str);
}

int WACC(atoi, 0)(char *str)
{
    return atoi(str);
}

char *WACC(strcat, 0)(char* str1, char *str2) {
    strcat(str1, str2);
    return str1;
}