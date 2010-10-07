#include <stdio.h>

int apply(int (*fp)(const char *, ...), int val) {
    (*fp)("%d\n", val);
}

int main() {
    apply(printf, 42);
    return 0;
}
