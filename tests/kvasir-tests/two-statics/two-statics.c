#include <stdio.h>

int main() {
    int y1 = global_function1(0);
    int y2 = global_function2(0);
    printf("%d + %d = %d\n", y1, y2, y1 + y2);
    return 0;
}

int func1(int x) {
    static char buffer[100];
    return 3;
}

int func2(int x) {
    static char buffer[100];
    return 4;
}

static int func3(int x) {
    static char buffer[100];
    return 5;
}
