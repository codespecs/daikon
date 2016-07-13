#include <stdlib.h>
#include <stdio.h>

int totalCost (int d, int base, int tax) {
  int year = 2005;
  if (d > 1000) {
    int shippingFee = 10;
    return base + tax + shippingFee;
  }
  else {
    return base + tax;
  }
}

int main() {
  int dist = 3000;
  int itemPrice = 50;
  int taxPrice = 3;

  int uninitializedStackVar;
  printf("&uninitializedStackVar = %p\n", &uninitializedStackVar);
  printf("First try  (stack):  %d\n", uninitializedStackVar);
  printf("Second try (stack): %d\n", uninitializedStackVar);
  printf("Third try  (stack):  %d\n", uninitializedStackVar);

  int* uninitializedHeapVarP =
    (int*)malloc(sizeof(*uninitializedHeapVarP));
  printf("uninitializedHeapVarP = %p\n", uninitializedHeapVarP);
  printf("First try  (heap):  %d\n", *uninitializedHeapVarP);
  printf("Second try (heap): %d\n", *uninitializedHeapVarP);
  printf("Third try  (heap):  %d\n", *uninitializedHeapVarP);

  totalCost (dist, itemPrice, taxPrice);

  totalCost (1, 2, 3);
  totalCost (10, 2, 3);
  totalCost (100, 2, 3);
  totalCost (1000, 2, 3);
  totalCost (10000, 2, 3);
  totalCost (100000, 2, 3);
  return 0;
}
