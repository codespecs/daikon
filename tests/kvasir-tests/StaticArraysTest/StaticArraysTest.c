#include <stdio.h>

static char trueGlobalStaticBuffer[200];

int f(char *arg, char **strings) 
{    
  return 12;
}

int b(short oneShort, short* manyShorts)
{
  return 42;
}

int func1(int x) {
    static char buffer[100];
    return 3;
}

int func2(int x) {
    static char buffer[100];
    return 4;
}

int main() 
{
  char *localStrings[] = {"apple", "banana", "carrot", "daikon", "eggplant", "fig", "grape", 0 };
  static char *staticStrings[] = {"STATIC apple", "STATIC banana", "STATIC carrot", "STATIC daikon", "STATIC eggplant", "STATIC fig", "STATIC grape", 0 };

  // Test out shorts because they are 2 bytes long:
  short localShorts [] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
  static short staticShorts [] = {101, 102, 103, 104, 105, 106, 107, 108, 109};
  int i;

  printf("staticStrings = %p, staticShorts = %p\n", staticStrings, staticShorts);

  f(localStrings[1], localStrings);

  staticStrings[5] = "MODIFIED STATIC fig";

  f(staticStrings[1], staticStrings);

  staticStrings[7] = "MODIFIED STATIC horseradish";

  b(localShorts[5], localShorts);

  staticShorts[2] = 8192;

  b(staticShorts[5], staticShorts);

  for (i = 0; i < 9; i++)
    staticShorts[i] = 42;

  return 0;
}
