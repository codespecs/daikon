// This file tests the pointer-type disambiguation feature of Kvasir

#include "DisambigTest.h"
#include <stdlib.h>

int* intPtr_P;
int* intPtr_A;

float* floatPtr_P;
float* floatPtr_A;

int** intPtrPtr_P;
int** intPtrPtr_A;

char** stringPtr_P;
char** stringPtr_A;

// Tests out the various .disambig modes for strings
char* string_S = "Hello world\n";
char* string_C = "Hello world\n";
char* string_A = "Hello world\n";
char* string_P = "Hello world\n";

unsigned char u_char_C = 'x';
unsigned char u_char_I = 'x';

char char_C = 'y';
char char_I = 'y';

int intArray_P[5] = {11, 11, 12, 13, 15};
int intArray_A[5] = {11, 11, 12, 13, 15};

char* globalStringArray[3] = {"globalString1",
			      "globalString2",
			      "globalString3"};

TestBuffer globalTB;

int main() {
  // 1 TestBuffer
  TestBuffer tb;
  // 5 TestBuffers
  TestBuffer* tbArray = (TestBuffer*)calloc(5, sizeof(*tbArray));

  int localInt = 31415;
  int localIntArray[8] = {3, 1, 4, 1, 5, 9, 2, 6};

  float localFloat = 3.1415;
  float localFloatArray[8] = {0.3, 0.1, 0.4, 0.1, 0.5, 0.9, 0.2, 0.6};

  char* localString = "3.1415926";
  char* localStringArray[9] = {"3", ".", "1", "4", "1", "5", "9", "2", "6"};

  int* localIntPtrArray[2];

  intPtr_P = &localInt;
  intPtr_A = localIntArray;

  floatPtr_P = &localFloat;
  floatPtr_A = localFloatArray;

  intPtrPtr_P = &intPtr_A;

  localIntPtrArray[0] = intPtr_A;
  localIntPtrArray[1] = intPtr_P; // This will NEVER show up because
  // it's a multi-dimensional array and only the first dimension is printed out

  intPtrPtr_A = localIntPtrArray;

  stringPtr_P = &localString;
  stringPtr_A = localStringArray;

  fooSingle(&tb);
  fooMultiple(tbArray, 5);

  return 0;
}
