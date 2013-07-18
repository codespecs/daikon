#include <stdio.h>
#include <unistd.h>

unsigned long long read_tsc(void) {
    unsigned long long tsc;
    asm volatile ("rdtsc" : "=A" (tsc));
    return tsc;
}

int main(int argc, char **argv) {
    unsigned long long t1, t2;
    double mhz;
    t1 = read_tsc();
    //printf("%llu\n", t1);
    t1 = read_tsc();
    sleep(1);
    t2 = read_tsc();
    //printf("%llu\n", t2);
    mhz = (double)(t2 - t1) / (1000 * 1000);
    //printf("Clock rate about %g MHz\n", mhz);
    return 0;
}
