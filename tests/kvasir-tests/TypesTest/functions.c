// functions.c
// TypesTest application exercises various C data types
// Philip Guo 3-9-04

#include <stdlib.h>
#include <stdio.h>

#include "functions.h"

// Note!!!  These functions may overflow or function improperly
// at times.  The purpose is not to demonstrate correctness, but
// rather to present interesting cases for function parameter and
// return value tracking.

Char returnCharSum(const Char a, Char b)
{
  return (a + b);
}

Short returnShortSum(const Short a, Short b)
{
  return (a + b);
}

Int returnIntSum(const UChar a, Short b)
{
  return (a + b);
}

// Initial observations!  Long variables are too BIG to store
// in EAX so it may be stored elsewhere.  Maybe a pointer to
// it is stored in EAX.  This returns the INCORRECT VALUE!
Long returnLongSum(Long a, Long b)
{
  Long tempA = a;
  tempA -= 100;
  tempA += 100;
  return (tempA + b);
}

// It may easily overflow!
UChar returnUCharProduct(UChar a, UChar b)
{
  UChar count;
  UChar total = 0;
  for (count = 0; count < b; count++)
    total = returnCharSum(total, a);
  return total;
}

UShort returnUShortProduct(UShort a, UShort b)
{
  UInt product = (a * b);
  return (UShort)product;
}

UInt returnUIntProduct(Char a, Short b)
{
  Char count;
  UInt total = 0;
  for (count = 0; count < a; count++)
    total = returnIntSum(total, b);
  return total;
}

ULong returnULongProduct(Long a, Int b)
{
  return (a * b);
}

double returnDoubleSum(const float a, double b)
{
  double tempA = a;
  double tempB = b;
  return (tempA + tempB) / 1000000;
}

float returnFloatProduct(float a, float b)
{
  return (a * b) / 1000000;
}
