int globalD = 40;
int globalE[] = {1, 2, 3, 4};

void foo(int* a, int* b, int* c, int* d, int* e) {}
//void bar(int a, int b, int c) {}

int main(int argc, char** argv) {
  int localA = 10;
  int localB = 20;
  int localC = 30;

  foo(&localA, &localB, &localC, &globalD, globalE);
  //  bar(localA, localB, localC);

  return 0;
}
