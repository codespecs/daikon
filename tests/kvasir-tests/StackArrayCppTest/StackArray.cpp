#include <stdlib.h>
#include <iostream>
#include <string.h>
#include "StackArray.h"

using namespace std;

int Stack::numStacksCreated;

Stack::Stack(char* name, int maxSize) {
  myName = strdup(name);
  maxElts = maxSize;
  numElts = 0;
  Stack::numStacksCreated++;
  stackRep = new int[maxSize];
}

void Stack::push(int dat) {
  if (numElts < maxElts) {
    numElts++;
    stackRep[numElts - 1] = dat;
  }
}

int Stack::peek() {
  if (numElts > 0)
    return stackRep[numElts - 1];
  else
    return 0; // Yeah, I know, no error handling :)
}

int Stack::pop() {
  if (numElts > 0) {
    privateStuff();
    numElts--;
    return stackRep[numElts];
  }
  else
    return 0; // Yeah, I know, no error handling :)
}

char* Stack::getName() {return myName;}

int Stack::getMaxElts() {return maxElts;}
int Stack::getNumElts() {return numElts;}

int Stack::getNumStacksCreated() {
  return Stack::numStacksCreated;
}

Stack::~Stack() {
  delete[] stackRep;
  free(myName);
}

int Stack::privateStuff() {
  cout << "\n!PRIVATE!";
  return 42;
}
