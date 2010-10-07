#include <stdio.h>
#include "Dstr.hh"

int main() {
    Dstr s1 = "Hello, world!";
    int len = s1.length();
    s1 += "\n";
    char aitch = s1[0];
    s1.lowercase();
    printf("%s", s1.aschar());
    return 0;
}
