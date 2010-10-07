// TypesTest.c
// TypesTest application exercises various C data types
// Philip Guo 3-9-04

#include <stdlib.h>
#include <stdio.h>

#include "functions.h"

union basicTypesUnion
{
  int int_value;
  char char_value;
  float float_value;
  double double_value;
};

struct bitFieldsStruct
{
  unsigned int a:1;
  unsigned int b:2;
  unsigned int c:3;
  unsigned int d:6;
  long e:10;
  short f:15;
};

void basicFunctionTest();

int main()
{
  //  printf("Types Test by Philip Guo (3-9-04)\n\n");
  basicFunctionTest();
  return 0;
}

// Test basic functions in functions.h
void basicFunctionTest()
{
  int i, j;

  for (i = 0; i < 10; i++) {
    UInt input[18] =
      {18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1};

    // For the sake of calculating comparability, make sure
    // that we pass in unique numbers to all of these functions
    // and not the SAME a or b
    returnCharSum((Char)input[0], (Char)input[1]);
    returnShortSum((Short)input[2], (Short)input[3]);
    returnIntSum((Int)input[4], (Int)input[5]);
    returnLongSum((Long)input[6], (Long)input[7]);
    returnUCharProduct((UChar)input[8], (UChar)input[9]);
    returnUShortProduct((UShort)input[10], (UShort)input[11]);
    returnUIntProduct(input[12], input[13]);
    returnULongProduct((Long)10000000, 10000000);
    returnDoubleSum(((float)input[14])/3, ((double)input[15])*2.718);
    returnFloatProduct(((float)input[16])*3.14159, ((float)input[17])*3.45);
  }
}
