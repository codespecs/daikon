#include <stdio.h>

#include "liba.h"
#include "libb.h"

int main(int argc, char **argv) {
    set_a(42);
    set_b(69);
    printf("Sum is %d\n", get_a() + get_b());
    return 0;
}
