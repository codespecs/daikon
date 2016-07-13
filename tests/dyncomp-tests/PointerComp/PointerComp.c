#include <stdlib.h>

void inc(int* a, int* b) {
  *a += *b;
}

void internalInteraction(int* x, int* y, int* z, int* oranges) {
  *z = *x + *y;
  *oranges++;
}

void interactAfter(int* x, int* y, int* z) {
}

int main() {
  int x = 5;
  int* yPtr = (int*)malloc(sizeof(*yPtr)); // malloc does create a new tag
  int* xPtr = &x; // &x does not create a new tag
  *yPtr = 10;
  inc(xPtr, yPtr);

  int a = 1, b = 2, c = 3, oranges = 10;

  internalInteraction(&a, &b, &c, &oranges);

  int d = 10, e = 15;

  xPtr = calloc(1, sizeof(*xPtr));

  interactAfter(&d, &e, xPtr);

  // Notice that e doesn't interact because it's the 'shift amount'
  d = *xPtr - d << e;

  return 0;
}
