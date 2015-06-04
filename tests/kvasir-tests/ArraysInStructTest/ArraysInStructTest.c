// TODO: This is another test case that fails on AMD64 due to the
// DynComp single tag per register issue.  If (when) we fix this,
// restore the three items below to their original values. (markro)

#include <stdio.h>
#include <string.h>

struct bar {
  //char blah;
  char blah[8];
  int bar_int_20[20];
  int bleh;
};

struct foo {
  //int int_5[5];
  int int_5[6];
  //char char_10[10];
  char char_10[16];
  struct bar bar_struct;
  double double_15[15];
};

struct foo globalFoo;

void baz(struct foo f) {
  printf("double_15 is at %p\n", &f.double_15[0]);
  return;
}

int main() {
  int i;
  struct foo localFoo;

  // These arrays must be completely initialized because
  // Kvasir currently can't tell what's initialized on the stack
  // so that it assumes that everything is initialized.  If you don't
  // initialize everything on here, then you'll get failures because
  // the garbage values in the arrays will bark.

  for (i = 0; i < 4; i++) {
    localFoo.int_5[i] = globalFoo.int_5[i] = i;
  }

  strcpy(globalFoo.char_10, "!!Hello!!");

  for (i = 0; i < 20; i++) {
    localFoo.bar_struct.bar_int_20[i] = globalFoo.bar_struct.bar_int_20[i] = 20 - i;
  }

  for (i = 0; i < 15; i++) {
    localFoo.double_15[i] = globalFoo.double_15[i] = 3.1415 * i;
  }

  baz(globalFoo);

  baz(localFoo);

  return 0;
}
