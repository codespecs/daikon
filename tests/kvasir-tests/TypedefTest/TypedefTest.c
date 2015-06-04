// Tests the use of typedefs to access unnamed struct variables

#include <stdio.h>
#include <stdlib.h>

// On x86-64 (AMD64) small struct arguments are pass by value in registers.
// However, DynComp (by design) only allocates a single tag per register.
// This causes 'a' and 'b' below to be marked with a false interaction.
// I'm making 'a' a long for now; but if we ever change this implementation
// (i.e., allocate byte tags for registers like we do for memory) then we
// should change it back to int. (markro)

typedef struct {
//int a;
  long a;
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
