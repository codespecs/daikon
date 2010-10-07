// This tests what qualify as interactions and what do not

// An interaction occurs between two variables when they are the
// operands of the following binary operators:

//    * arithmetic: *, /, %, +, -
//    * relational: <, >, <=, >=, ==, !=
//    * bitwise logical: &, |, ^

// Do logical (&&, ||, ?) imply interaction?
// NO

// Do shift operators (<<, >>)?
// Kind of.  If z = x << y, then x and z are comparable, but not y

void add(int a, int b) {};
void sub(int a, int b) {};
void mult(int a, int b) {};
void div(int a, int b) {};
void mod(int a, int b) {};

void bitAnd(int a, int b) {};
void bitOr(int a, int b) {};
void bitXor(int a, int b) {};

void lShift(int a, int b, int c) {};
void rShift(int a, int b, int c) {};

void lessThan(int a, int b, char res) {};
void greaterThan(int a, int b, char res) {};
void lessThanOrEqualTo(int a, int b, char res) {};
void greaterThanOrEqualTo(int a, int b, char res) {};

void lessThanFloat(float a, float b, char res) {};

void logicalAnd(char a, char b) {};
void logicalOr(char a, char b) {};
void logicalTernaryT(char blah, char a, char b, char c) {};
void logicalTernaryF(char blah, char a, char b, char c) {};

void add16(short a, unsigned short b) {};
void sub16(short a, unsigned short b) {};
void mult16(short a, unsigned short b) {};
void div16(short a, unsigned short b) {};
void mod16(short a, unsigned short b) {};

int main() {
  int test[16] =
    {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
  int result;

  result = test[0] + test[1];
  add(test[0], test[1]);

  result = test[2] - test[3];
  sub(test[2], test[3]);

  result = test[4] * test[5];
  mult(test[4], test[5]);

  result = test[6] / test[7];
  div(test[6], test[7]);

  result = test[8] % test[9];
  mod(test[8], test[9]);

  result = test[10] % test[11];
  bitAnd(test[10], test[11]);

  result = test[12] % test[13];
  bitOr(test[12], test[13]);

  result = test[14] % test[15];
  bitXor(test[14], test[15]);

  int x = 1, y = 3, z;
  z = x << y;
  lShift(x, y, z);

  int j = 20000, k = 4, m;
  m = j >> k;
  rShift(j, k, m);

  int apple1 = 10, apple2 = 100;

  if (apple1 < apple2) {
    lessThan(apple1, apple2, (apple1 < apple2));
  }

  apple1 = 1000, apple2 = -5;
  if (apple1 > apple2) {
    greaterThan(apple1, apple2, (apple1 > apple2));
  }

  apple1 = -10, apple2 = -5;
  char lte = (apple1 <= apple2);
  if (lte) {
    lessThanOrEqualTo(apple1, apple2, lte);
  }

  apple1 = 100, apple2 = 0;
  char gte = (apple1 >= apple2);
  if (gte) {
    greaterThanOrEqualTo(apple1, apple2, gte);
  }

  double doubleA = 10.231;
  float floatB = 10.234;

  if (doubleA < floatB) {
    lessThanFloat((float)doubleA, floatB, (doubleA < floatB));
  }

  int c1 = 0, c2 = 1;

  if (c1 || c2) {
    logicalOr(c1, c2);
  }

  char c3 = 'x', c4 = 'y';

  if (c3 && c4) {
    logicalAnd(c3, c4);
  }

  short testShort[5] = {-1, -2, -3, -4, -5};
  unsigned short testUShort[5] = {1, 2, 3, 4, 5};

  char t1 = 1, t2 = 3, t3 = 5;

  logicalTernaryT((t1 ? t2 : t3), t1, t2, t3);

  t1 = 0, t2 = 10, t3 = 15;
  logicalTernaryF((t1 ? t2 : t3), t1, t2, t3);

  result = testShort[0] + testUShort[0];
  add16(testShort[0], testUShort[0]);

  result = testShort[1] + testUShort[1];
  sub16(testShort[1], testUShort[1]);

  result = testShort[2] + testUShort[2];
  mult16(testShort[2], testUShort[2]);

  result = testShort[3] + testUShort[3];
  div16(testShort[3], testUShort[3]);

  result = testShort[4] + testUShort[4];
  mod16(testShort[4], testUShort[4]);

  return 0;
}
