// functions.h
// TypesTest application exercises various C data types
// Philip Guo 3-9-04

#ifndef FUNCTIONS_H
#define FUNCTIONS_H

typedef unsigned char          UChar;
typedef unsigned short         UShort;
typedef unsigned int           UInt;
typedef unsigned long long int ULong;

typedef signed char            Char;
typedef signed short           Short;
typedef signed int             Int;
typedef signed long long int   Long;

Char returnCharSum(Char a, Char b);
Short returnShortSum(Short a, Short b);
Int returnIntSum(UChar a, Short b);
Long returnLongSum(Long a, Long b);

UChar returnUCharProduct(UChar a, UChar b);
UShort returnUShortProduct(UShort a, UShort b);
UInt returnUIntProduct(Char a, Short b);
ULong returnULongProduct(Long a, Int b);

double returnDoubleSum(float a, double b);
float returnFloatProduct(float a, float b);

#endif
