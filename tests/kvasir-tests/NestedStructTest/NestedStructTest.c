// Kvasir unit test for nested structs

// TODO: This is another test case that fails on AMD64 due to the
// DynComp single tag per register issue.  If (when) we fix this,
// change all the 'long's below back to 'int'. (markro)

#include <stdio.h>
#include <stdlib.h>

struct foo {
  long age;
  struct bar {
    long a;
    long b;
  } b;
  char* name;
};

struct betterFoo {

  long betterAge;

  struct betterBar {
    long betterA;
    struct foo crappyFoo;

    struct bazzz {
      char hello[100];
      char world[1000];
    } myBazzz;

    long betterB;
  } namedBar;

  char* betterName;
};

struct foo globalFoo[4];
struct betterFoo globalBetterFoo;

struct foo* returnF(struct foo* f, long* blah)
{
  return f;
}

int main() {
  struct foo fooArray[20];
  long intArray1[100];
  long* onHeap = (long*)calloc(69, sizeof(*onHeap));

  globalFoo[0].age = 13;
  globalFoo[1].age = 23;
  globalFoo[2].age = 33;
  globalFoo[3].age = 43;

  globalFoo[0].b.a = 0;
  globalFoo[1].b.a = 1;
  globalFoo[2].b.a = 2;
  globalFoo[3].b.a = 3;

  globalFoo[0].b.b = 0;
  globalFoo[1].b.b = 100;
  globalFoo[2].b.b = 200;
  globalFoo[3].b.b = 300;

  globalFoo[0].name = "globalFoo[0]";
  globalFoo[1].name = "globalFoo[1]";
  globalFoo[2].name = "globalFoo[2]";
  globalFoo[3].name = "globalFoo[3]";

  returnF(fooArray, intArray1);
  printf("&globalFoo: %p, &globalBetterFoo: %p, &fooArray: %p\n", &globalFoo, &globalBetterFoo, fooArray);
  returnF(globalFoo, onHeap);
  return 0;
}
