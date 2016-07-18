#include <stdlib.h>

typedef struct {
  unsigned short a;
  double b;
  unsigned short* cPtr;
} record;

void structByValue(record x, record y, double d) {}
void structByPtr(record* xPtr, record* yPtr, double* dPtr) {}
void assignPtrThenFieldsInteract(record* aPtr, record* bPtr, unsigned char c) {}
void assignPtrAwayBeforeFieldsInteract(record* aPtr, record* bPtr) {}


int main() {
  record one, two;
  double localDouble = 1.234567;
  one.a = 5;
  one.b = localDouble;
  one.cPtr = &one.a;

  two = one;

  structByValue(one, two, localDouble);
  structByPtr(&one, &two, &localDouble);

  // Assign barPtr to fooPtr, then have the fields
  // of fooPtr interact.  The fields of barPtr should
  // thus also interact.
  record* fooPtr = (record*)malloc(sizeof(*fooPtr));
  record* barPtr = (record*)malloc(sizeof(*barPtr));

  int c = 10;

  barPtr = fooPtr;

  fooPtr->b = c;
  fooPtr->a = fooPtr->b;

  assignPtrThenFieldsInteract(fooPtr, barPtr, c);

  // Now assign barPtr to fooPtr, then assign it away
  // to something else, and have the fields of fooPtr
  // interact.  The fields of barPtr should NOT interact.

  fooPtr = (record*)malloc(sizeof(*fooPtr));
  barPtr = (record*)malloc(sizeof(*barPtr));

  barPtr = fooPtr;

  barPtr = &one;

  fooPtr->b = 3.14159;
  fooPtr->a = fooPtr->b;

  assignPtrAwayBeforeFieldsInteract(fooPtr, barPtr);

  return 0;
}
