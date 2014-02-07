#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int f(char *arg, char **strings) {
    return 12;
}

char *uppercase (char *s) {
    static char upcasestr[128];
    int i;

    for (i = 0; i < strlen (s); i++)
	upcasestr[i] = toupper(s[i]);
    upcasestr[i] = '\0';

    return (upcasestr);
}

int main()
{
    char *none = 0;
    static char *strings[] = {"apple", "banana", "carrot", "daikon",
			      "eggplant", "fig", "grape", 0 };
    f(strings[1], strings);

    uppercase("huckleberry");

    return 0;
}
