// This tests various function names to try out for the
// --ppt-list-file program point file format.  The desire is to be
// able to uniquely identify a function at x86-IR translation time.
// Using start PC addresses is not reliable because they change upon
// target program recompilation.

// Things to watch out for:

// 1.) C++ mangled function names
// 2.) C++ overloaded functions
// 3.) File static function names - 2 functions in different files
//     can have the same name if they are declared as static

#include "second_file.h"

#include <stdlib.h>

int* globalIntArray;
int* anotherGlobalIntArray;

static void staticFoo(int x, int y) {}

void firstFileFunction(int blah) {
  int i;
  globalIntArray = (int*)malloc(10 * sizeof(*globalIntArray));

  staticFoo(1, 2);

  for (i = 0; i < 10; i++)
    globalIntArray[i] = i + 1;
}

int main() {
  firstFileFunction(5);

  secondFileFunction();
  return 0;
}
