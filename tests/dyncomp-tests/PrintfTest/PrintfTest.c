// This tests to make sure that variables passed as arguments to the
// same printf function call don't automatically count as comparable,
// as they did in the version of DynComp of 2005-06-09

#include <stdio.h>

void printfIntInt(int a, int b) {
  //  int z = 0xBEEF;
  printf("a=%d, b=%d\n", a, b);
  //  bar("a=%d, b=%f\n", a, b);
}

void printfIntFloat(int a, int b) {
  printf("a=%d, b=%d\n", a, b);
}

void printfIntFloatUInt(int a, int b, int c) {
  printf("a=%d, b=%d, c=%u\n", a, b, c);
}

void printfIntFloatUIntInt(int a, int b, int c, int d) {
  printf("a=%d, b=%d, c=%u, d=%d\n", a, b, c, d);
}

int main() {
  int a = 0xAAAA;
  int b = 0xBBBB;
  int c = 0xCCCC;
  int d = 0xDDDD;

  printfIntInt(a, b);
  printfIntFloat(a, b);
  printfIntFloatUInt(a, b, c);
  printfIntFloatUIntInt(a, b, c, d);

  return 0;
}
