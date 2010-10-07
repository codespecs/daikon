// A stack of ints implemented as an array

#include <iostream>
#include <stdlib.h>

using namespace std;

#ifndef STACK_ARRAY_H
#define STACK_ARRAY_H

class Stack {
 public:
  Stack(char* name, int maxSize);
  ~Stack();

  void push(int dat);
  int peek();
  int pop();

  char* getName();

  int getMaxElts();
  int getNumElts();

  static int getNumStacksCreated();

 private:
  int numElts;
  int maxElts;
  char* myName;
  int* stackRep;

  static int numStacksCreated;

  int privateStuff();
};

#endif // STACK_ARRAY_H
