// DynComp test for handling of function parameters

// The parameters should be comparable because they interacted BEFORE
// the call (derived comparability):
void intComp(int a, int b, int c) {
  a += 5; b -= 6; c *= 7;
}

// The parameters should not be comparable because they never
// interacted either before, after, or inside the call:
void intNonComp(int a, int b, int c) {
  a += 5; b -= 6; c *= 7;
}

void floatNonComp(float a, float b, float c) {
  a += 5; b -= 6; c *= 7;
}

void floatIntComp(int a, double b) {
  a /= 5; b *= 3;
}

// x and y should be comparable but not z because the VALUE of z at
// function entrance isn't comparable to the values of x and y.  z
// gets assigned a NEW value which is comparable, but because we use
// the virtual stack to only capture the pre-state, the z we get at
// the end of the function is the same z at the beginning
int intCompInternal(int x, int y, int z) {
  z = x + y;
  return z + 2;
}

float floatCompInternal(float x, float y, float z) {
  z = x + y;
  return z + 2;
}

// {a, b, c} should be comparable because they interact AFTER
// the function call
void intCompAfter(int a, int b, int c) {
}

void floatCompAfter(float a, float b, float c) {
}

int main() {
  int i;
  int lotsOfInts[12] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12};

  int myApples = 10;
  int yourApples = 5;
  int ourApples = myApples + yourApples;

  double myFloat = ourApples / yourApples;

  intComp(myApples, yourApples, ourApples);
  floatIntComp(myApples, myFloat);

  // The individual elements of the array should not be comparable to
  // one another:
  intNonComp(lotsOfInts[0], lotsOfInts[1], lotsOfInts[2]);
  intNonComp(lotsOfInts[3], lotsOfInts[4], lotsOfInts[5]);
  intNonComp(lotsOfInts[6], lotsOfInts[7], lotsOfInts[8]);
  intNonComp(lotsOfInts[9], lotsOfInts[10], lotsOfInts[11]);

  // These become comparable due to interaction INSIDE the function
  float x = 0.234, y = 0.432, z = 0.101, w;
  w = floatCompInternal(x, y, z);

  int a = 1, b = 2 , c = 3, d;
  d = intCompInternal(a, b, c);

  // Grab new values for myApples, yourApples, and ourApples and pass
  // them into intNonComp.  These new values are non-comparable so
  // that intNonComp's params are still not comparable.
  myApples = 100;
  yourApples = 200;
  ourApples = 300;
  intNonComp(myApples, yourApples, ourApples);

  // Ditto for floats:
  x = 1.234;
  y = 2.345;
  z = 3.456;
  floatNonComp(x, y, z);

  floatNonComp((float)lotsOfInts[0], (float)lotsOfInts[1], (float)lotsOfInts[2]);
  floatNonComp((float)lotsOfInts[3], (float)lotsOfInts[4], (float)lotsOfInts[5]);
  floatNonComp((float)lotsOfInts[6], (float)lotsOfInts[7], (float)lotsOfInts[8]);

  // Now let's test some stuff which becomes comparable due to
  // interactions which occur AFTER the function call:
  // (This is where the extra round of propagations at the end is crucial)

  // Re-assign fresh new values:
  a = 1;
  b = 2;
  c = 3;

  intCompAfter(a, b, c);

  x = 0.1;
  y = 0.2;
  z = 0.3;

  floatCompAfter(x, y, z);

  // Now let's have the params interact
  d = a / b * c;
  w = x - (y + z);

  return 0;
}
