#ifndef CPP_FUNCTIONT_TEST
#define CPP_FUNCTIONT_TEST

int overloaded_foo(int a, int b);
int overloaded_foo(double a, double b);
int overloaded_foo(char* a, char* b);
int overloaded_foo(int a);

void pass_by_reference(int &ref_a, int val_b);
void pass_ptr_by_reference(int* &ref_a);
void default_args_func(int a, int b, int c);

int& return_reference(int &a);

#endif
