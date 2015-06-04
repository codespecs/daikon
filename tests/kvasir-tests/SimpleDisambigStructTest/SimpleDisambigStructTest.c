// This tests if the .disambig 'P' option can really reveal
// more about structs than is possible without .disambig
// ie. does it really give us one extra layer of sequences
// so that we can display arrays within structs?

// TODO: This is another test case that fails on AMD64 due to the
// DynComp single tag per register issue.  If (when) we fix this,
// change the size of 'staticString' back to 10. (markro)

#include <stdlib.h>
#include <string.h>

typedef struct {
  int* dynamicIntArray;   // {0, 1, 2, 3, 4}
  int staticIntArray[10]; // {10, 9, 8, 7, 6, 5, 4, 3, 2, 1}
  char* dynamicString;    // "Hello world \n"
  char staticString[16];  // "Hello1234"
  int number;             // 42
} Buffer;

// b is always 1 Buffer object
void foo(Buffer* b) {
  int i;
  b->dynamicIntArray = (int*)calloc(5, sizeof(int));
  for (i = 0; i < 5; i++) {
    b->dynamicIntArray[i] = i;
  }
  for (i = 0; i < 10; i++) {
    b->staticIntArray[i] = (10 - i);
  }
  b->dynamicString = strdup("Hello world\n");
  strcpy(b->staticString, "Hello1234");
  b->number = 42;
}

void bar(Buffer actualB) {

}

int main() {
  Buffer* b = (Buffer*)malloc(sizeof(*b));
  foo(b);
  bar(*b);
  return 0;
}
