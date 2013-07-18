int **global;

void f(int ***ppp) { return; }

int main()
{
  long x = 1;
  int *ptr;
  int **ptrptr = &ptr;

  global = (int **)&x;

  f(&ptrptr);
  return 0;
}
