// Tests the use of typedefs to access unnamed struct variables

#include <stdio.h>

typedef struct {
  int a;
  char b;
  double c;
} anonymousStruct;

enum {MON, TUES, WED} unnamedGlobalEnum;

typedef unsigned int ROUTEBAGA;

static anonymousStruct* createCopy(anonymousStruct a, ROUTEBAGA x) {
  anonymousStruct* copy = (anonymousStruct*)(malloc(sizeof(*copy)));
  *copy = a;
  return copy;
}

anonymousStruct globalStruct;

int main()
{
  anonymousStruct* myCopy;

  globalStruct.a = 5;
  globalStruct.b = 'b';
  globalStruct.c = 3.14159;

  myCopy = createCopy(globalStruct, 102);
  return 0;
}
