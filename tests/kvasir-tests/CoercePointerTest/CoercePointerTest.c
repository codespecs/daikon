// Test of the Kvasir .disambig option for coercing pointers into
// types different than their declared types.  Useful for handling
// C faux-polymorphism

#include <stdlib.h>
#include <string.h>

typedef struct {
  int foo_int;
  char* foo_str;
  double foo_double;
  void* foo_baz; // This should be observed as type baz
} foo;

struct bar {
  int bar_int;
};

typedef struct {
  int baz_int;
  char baz_str[10];
} baz;

void normal_view(foo* f) {}

// Observe f as a pointer to type foo
void view_foo(void* f) {}
// Observe b as a pointer to type bar
void view_bar(void* b) {}

void view_foo_and_bar(void* f, void* b) {}

// We need to have this here so that Kvasir recognizes that some
// variable has type baz so that it creates an entry for it in
// DaikonTypesTable.  We need to fix this in the future.
baz junk;

// Observe bar_as_foo as a pointer to type foo
void view_bar_as_foo(struct bar* bar_as_foo) {}

int main() {
  foo* f = (foo*)malloc(sizeof(*f));
  struct bar* b = (struct bar*)malloc(sizeof(*b));

  f->foo_int = 42;
  f->foo_str = (char*)strdup("f->foo_str");
  f->foo_double = 2.718;
  f->foo_baz = (void*)malloc(sizeof(baz));
  ((baz*)(f->foo_baz))->baz_int = 103083;
  strcpy(((baz*)(f->foo_baz))->baz_str, "TEN_BYTES");

  b->bar_int = 123456;

  normal_view(f);
  view_foo(f);
  view_bar(b);
  view_foo_and_bar(f, b);
  view_bar_as_foo((struct bar*)f);

  return 0;
}
