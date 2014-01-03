#include <stdio.h>
#include <stdlib.h>

int globalX = 5;
char* whatsUp = "what's up dawg!!!";

int globalIntArray[18] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18};

char**** returnIntSum(int* u, float* a, char* str, char*** dummy, double** b, int uninit)
{
  int c = **b;
  int foo = uninit;
  int bar = *a;
  **b *= 2;

  *a = 2.718;

  if (u)
    returnIntSum(0, a, str + 3, dummy, b, uninit);
  else
    return (char****)0xDEADDEAD;

  return (char****)0xABBABEEF;
}

int main()
{
  float a;
  double x;
  double* xPtr = &x;
  char** ridiculous = &whatsUp;
  int* undef = (int*)(0xDEADBEEF);
  char* uninitString = (char*)(0xABBADABA);
  int uninit;
  int init = 5;
  int stackArray13[13] = {13,12,11,10,9,8,6,5,4,3}; // Only 10 elts. init
  int* intArray = (int*)calloc(8, sizeof(int));
  int* intArrayUninit = (int*)malloc(38*sizeof(int));

  //  printf("&globalX=%p\n", &globalX);
  //  printf("&&whatsUp=%p, &whatsUp=%p, whatsUp=%p\n", &ridiculous, &whatsUp, whatsUp);

  returnIntSum(stackArray13, &a, whatsUp, &ridiculous, &xPtr, uninit);
  //  returnIntSum(undef, &a, uninitString, &ridiculous,  &xPtr, init);
  //  returnIntSum(globalIntArray, &a, whatsUp, &ridiculous, &xPtr, uninit);

  return 0;
}
