// Tests the following C++ features:
// 1. Function overloading
// 2. Pass parameters by reference
// 3. Default values for function arguments
// 4. 'const' modifiers

#include "CppFunctionTest.h"

int globalA = 1000;
int globalB = 2000;

int overloaded_foo(int a, int b) {
  return a + b;
}

int overloaded_foo(double a, double b) {
  return (int)(a + b);
}

int overloaded_foo(char* a, char* b) {
  return (int)(a[0] + b[0]);
}

int overloaded_foo(int a) {
  return 42;
}

void pass_by_reference(int &ref_a, int val_b) {
  ref_a++;
  val_b++;
}

void pass_ptr_by_reference(int* &ref_a) {
  ref_a = &globalB;
}

int& return_reference(int &a) {
  a = -999;
  return a;
}

void pass_by_const_reference(const int& cref_a) {
}

void default_args_func(int a, int b = 5, int c = 10) {
}

int main() {
  int i_x = 1, i_y = 2;
  double d_x = 1.56, d_y = 2.15;
  char *str_x = (char*)"hello", *str_y = (char*)"world";

  overloaded_foo(i_x, i_y);
  overloaded_foo(d_x, d_y);
  overloaded_foo(str_x, str_y);
  overloaded_foo(100);

  int a = 10;
  int b = 10;

  int* ref_a = &globalA;

  pass_by_reference(a, b);
  pass_by_reference(a, b);
  pass_by_reference(a, b);
  pass_by_reference(a, b);
  pass_by_reference(a, b);

  default_args_func(1);
  default_args_func(1, 2);
  default_args_func(1, 2, 3);

  pass_ptr_by_reference(ref_a);

  pass_by_const_reference(a);

  return_reference(a);

  return 0;
}
