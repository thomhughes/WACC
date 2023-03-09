#include <string.h>
#include <stdlib.h>
#include "wacc.h"

int WACC(strlen)(char *str)
{
    return strlen(str);
}

int WACC(atoi)(char *str)
{
    return atoi(str);
}

char *WACC(strcat)(char* str1, char *str2) {
    strcat(str1, str2);
    return str1;
}