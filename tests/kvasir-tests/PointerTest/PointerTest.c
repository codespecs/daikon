// Pointer test program for Valgrind

// TODO: This is another test case that fails on AMD64 due to the
// DynComp single tag per register issue.  If (when) we fix this,
// change the 'long's below back to 'int'. (markro)

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct point {
  long x;
  long y;
};

struct point makepoint(long x, long y)
{
  struct point temp;

  printf("%p\n", &temp);

  temp.x = x;
  temp.y = y;
  printf("(%i,%i)\n", temp.x, temp.y);
  return temp;
}

void writeCrap(char* s)
{
  *s = 'p';
  *(s+1) = 'a';
  *(s+2) = 'r';
  *(s+3) = 'c';
}

int main()
{
  struct point origin, tenfive, *pointPtr, *basePtr;
  char buffer [100];
  char* offset;
  int i;

  origin = makepoint(34,45);
  printf("%p\n", &origin);
  printf("(%i,%i)\n", origin.x, origin.y);
  printf("\n");

  tenfive = makepoint(10,5);
  printf("%p\n", &tenfive);
  printf("(%i,%i)\n", tenfive.x, tenfive.y);

  printf("Size of struct point: %i\n", sizeof(struct point));

  pointPtr = malloc(5*(sizeof(struct point)));

  basePtr = pointPtr + 100;

  printf("pointPtr = %p\n", pointPtr++);
  printf("pointPtr + 1 = %p\n", pointPtr++);
  printf("pointPtr + 2 = %p\n", pointPtr++);
  printf("pointPtr + 3 = %p\n", pointPtr++);
  printf("pointPtr + 4 = %p\n", pointPtr);

  basePtr -= 100;

  free(basePtr);

  memset(buffer, 0, 100);
  writeCrap(buffer);
  offset = buffer + 20;
  writeCrap(offset);
  offset += 30;
  writeCrap(offset);

  for (i = 0; i < 100; i++) {
    char temp = buffer[i];
    if (temp >= 'a' && temp <= 'z') {
      printf("%c", temp);
    }
    else {
      printf("_");
    }
  }

  printf("\n");

  return 0;
}
