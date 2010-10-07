#include <stdio.h>
#include <stdlib.h>

typedef long long int rhubarbInt64;
typedef unsigned long long int UrhubarbInt64;
typedef unsigned int* UIntPtr;

char* globalStr = "MR FURLEY\t\nis a good man\rhello\tworld\t\n\n\n\\ \r \r a\r  \\b \\c";
unsigned char* localAddr;
int globalInt = 5555;
double globalDouble = 12345.67890123;
unsigned long long int globalLong = 6969696969696969;
static const int globalWkdayA = 4;

char* returnChar(char** charPtr)
{
  return 0;
}

int main()
{
  printf("char* globalStr: %p\n", &globalStr);
  printf("unsigned char* localAddr: %p\n", &localAddr);
  printf("int globalInt: %p\n", &globalInt);
  printf("double globalDouble: %p\n", &globalDouble);
  printf("unsigned long long int globalLong: %p\n", &globalLong);
  printf("static const int globalWkdayA: %p\n", &globalWkdayA);

  return 0;
}
