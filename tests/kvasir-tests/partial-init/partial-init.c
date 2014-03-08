#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

void print_bits(int len, unsigned char *bits) {
    int i;
    char c;
    unsigned char *p = bits;

    fflush(NULL);
    for (i = len; i > 0; i -= 8) {
        int j;
	unsigned char b = *p++;
        for (j = 0; j < min(i, 8); j++) {
	    c = '0' + (b & 1);
	    write(1, &c, 1);
	    b >>= 1;
	}
	c = ' ';
	write(1, &c, 1);
    }
    c = '\n';
    write(1, &c, 1);
    fflush(NULL);
}

void test_bits(unsigned char *p) {
    p[0] = 10; p[1] = p[2] = p[3] = 0;
    p[4] &= ~15;
    p[4] |= 10;
    print_bits(36, p);
    print_bits(64, p);
}

unsigned char global[8];

int main(int argc, char **argv) {
    unsigned char *dynamic = malloc(8);
    unsigned char stack[8];
    printf("Global:\n");
    test_bits(global);  /* All valid */
    printf("Dynamic:\n");
    test_bits(dynamic); /* Invalid, but probably 0 anyway */
    printf("Stack:\n");
    test_bits(stack);   /* Invalid and likely garbage */
    return 0;
}
