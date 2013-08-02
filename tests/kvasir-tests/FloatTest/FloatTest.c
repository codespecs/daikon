#include <stdio.h>
#include <stdlib.h>

double globalDouble[] = {123e-6, 123e1, 123e10, 123e100, 123e1000};

float floatMultiply(float a, float b) {
  return a*b;
}

float floatDivide(float a, float b) {
  return a/b;
}

float floatAdd(float a, float b) {
  return a+b;
}

float floatSub(float a, float b) {
  return a-b;
}

double doubleMultiply(double a, double b) {
  return a*b;
}

double doubleDivide(double a, double b) {
  return a/b;
}

double doubleAdd(double a, double b) {
  return a+b;
}

double doubleSub(double a, double b) {
  return a-b;
}


int main()
{

  // Test large(~ 10^100), medium, and small inputs
  floatMultiply(10e12, 12e23);
  floatDivide(1.0, 254);
  floatAdd(100.345, 34.456);
  floatSub(34, -3415.234242);

  doubleMultiply(10e130, 54e45);
  doubleDivide(1.0, 541234);
  doubleAdd(450.3451516134, 64.123413151);
  doubleSub(15, -5246.5415143232);
    
  return 0;
}
