#include <stdio.h>

const int DIM1 = 10;
const int DIM2 = 20;

struct boo {
  int a;
  int b;
  int c[40][50];
  int* d[90][70];
};

struct boo globalBoo;

void fint(int param[DIM1][DIM2]) {
  return;
}

void ffixedint(int param[987][654][321]) {};

void __attribute__(()) fcrashint(int param[10000][321], struct boo booParam) {
  printf("fcrashint: &param=%p &booParam=%p, diff=%d\n",
         &param, &booParam, (void*)&booParam - (void*)&param);
};

void fchar(char p[DIM1][DIM2]) {
  return;
}

int main() {
  int x[DIM1][DIM2];
  int i, j;
  for (i = 0; i < DIM1; i++)
    for(j = 0; j < DIM2; j++)
      x[i][j] = i+j*i;
  globalBoo.a = 42;
  globalBoo.b = 9876;
  globalBoo.c[0][0] = 12345;
  globalBoo.c[1][0] = 54321;
  globalBoo.c[2][0] = 42;
  globalBoo.c[9][49] = 949;
  fint(x);
  fcrashint(x, globalBoo);
  printf("sizeof(struct boo): %d\n", sizeof(struct boo));
  return 0;
}
