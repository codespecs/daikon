#include <stdio.h>
#include <stdlib.h>

#define NUM_COUNTERS 10

typedef struct _counter {
  int a;
} counter;

counter* counters;

int sum = 0;

// The invariant sum = sum(counter.a[]) should
// hold at every call to check()
void check() {
}

int main() {
  counters = calloc(1, NUM_COUNTERS*sizeof(counter));
  check();

  counters[0].a++;
  sum++;
  check();

  counters[5].a++;
  sum++;
  check();

  counters[9].a = 100;
  sum += 100;
  check();

  counters[0].a+= 2;
  sum+= 2;
  check();

  return 0;
}

