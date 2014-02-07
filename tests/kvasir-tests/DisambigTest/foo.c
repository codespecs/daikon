#include "DisambigTest.h"
#include <stdlib.h>

// Same as fooMultiple except for one tb only
void fooSingle(TestBuffer* tb) {
  int i = 0;

  tb[i].intPtr_P = (int*)malloc(sizeof(int));
  *(tb[i].intPtr_P) = (i + 1);

  tb[i].intPtr_A = (int*)malloc(4 * sizeof(int));
  tb[i].intPtr_A[0] = (i + 1);
  tb[i].intPtr_A[1] = (i + 1) * 10;
  tb[i].intPtr_A[2] = (i + 1) * 100;
  tb[i].intPtr_A[3] = (i + 1) * 1000;

  tb[i].stringPtr_P = &string_S;
  tb[i].stringPtr_A = globalStringArray;

  tb[i].string_S = string_S;
  tb[i].string_C = string_C;
  tb[i].string_A = string_A;
  tb[i].string_P = string_P;

  tb[i].char_C = 'a' + (i + 1);
  tb[i].char_I = 'a' + (i + 1);

  globalTB = *tb;
}

void fooMultiple(TestBuffer* tb, int numElts) {
  int i;

  for (i = 0; i < numElts; i++) {
    tb[i].intPtr_P = (int*)malloc(sizeof(int));
    *(tb[i].intPtr_P) = (i + 1);

    tb[i].intPtr_A = (int*)malloc(4 * sizeof(int));
    tb[i].intPtr_A[0] = (i + 1);
    tb[i].intPtr_A[1] = (i + 1) * 10;
    tb[i].intPtr_A[2] = (i + 1) * 100;
    tb[i].intPtr_A[3] = (i + 1) * 1000;

    tb[i].stringPtr_P = &string_S;
    tb[i].stringPtr_A = globalStringArray;

    tb[i].string_S = string_S;
    tb[i].string_C = string_C;
    tb[i].string_A = string_A;
    tb[i].string_P = string_P;

    tb[i].char_C = 'a' + (i + 1);
    tb[i].char_I = 'a' + (i + 1);
  }
}
