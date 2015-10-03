#include <stdlib.h>
#include <stdio.h>

void foo(void) { }

static int *leaf_global1, *leaf_global2, *leaf_global3, *leaf_global4;
void leaf(void) {
  int local1 = 1, local2 = 2;
  leaf_global1 = leaf_global2 = &local1;
  leaf_global3 = &local2;
  leaf_global4 = &local1;
}

int *nonleaf_global1, *nonleaf_global2, *nonleaf_global3, *nonleaf_global4;
void nonleaf(void) {
  int local1 = 1, local2 = 2;
  //printf("");
  nonleaf_global1 = nonleaf_global2 = &local1;
  nonleaf_global3 = &local2;
  nonleaf_global4 = &local1;
}

int* intPtr;
int** intPtrPtr;

float* floatPtr1;
float* floatPtr2;

int main() {
  //char *unused = malloc(40);

  int localIntArray[8] = {3, 1, 4, 1, 5, 9, 2, 6};

  float localFloat = 3.1415;
  float localFloatArray[8] = {0.3, 0.1, 0.4, 0.1, 0.5, 0.9, 0.2, 0.6};

  intPtr = localIntArray;

  floatPtr1 = &localFloat;
  floatPtr2 = localFloatArray;

  intPtrPtr = &intPtr;

  foo();

  leaf();
  nonleaf();

  return 0;
}

