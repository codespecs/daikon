void foo(int a, int b, int c) {
  a += 5; b -= 6; c *= 7;
}

void bar(int a, int b, int c) {
  a += 5; b -= 6; c *= 7;
}

int add(int a, int b) {
  return a + b;
}

int main() {
  int myApples = 10;
  int yourApples = 5;
  int ourApples = myApples + yourApples;
  int myOranges = 17;

  foo(myApples, yourApples, ourApples);
  bar(myApples, myOranges, yourApples);

  foo(1, 2, 3);
  bar(4, 5, 6);

  myOranges = 10;

  add(myApples, myOranges);
  return 0;
}
