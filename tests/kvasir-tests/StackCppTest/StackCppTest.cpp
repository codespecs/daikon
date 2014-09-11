// Adapted for Kvasir regression tests by Philip Guo

//: C04:StackTest.cpp
// From Thinking in C++, 2nd Edition
// Available at http://www.BruceEckel.com
// (c) Bruce Eckel 2000
// Copyright notice in Copyright.txt
//{L} Stack
//{T} StackTest.cpp
// Test of nested linked list
#include "Stack.h"
#include <iostream>
using namespace std;

int main() {
  Stack first((char*)"My first stack");

  first.push((char*)"First line");
  first.push((char*)"Second line");
  first.push((char*)"Third line");
  first.push((char*)"Fourth line");
  first.push((char*)"Fifth line");

  // Pop the lines from the Stack and print them:
  char* s;

  cout << first.getName() << ":" << endl;

  while((s = first.pop()) != 0) {
    cout << s << endl;
  }

  cout << "numStacksCreated: " << Stack::getNumStacksCreated() << endl;
  cout << "publicNumLinksCreated: " << Stack::publicNumLinksCreated << endl;

  Stack second((char*)"My second stack");

  second.push((char*)"Uno");
  second.push((char*)"Dos");
  second.push((char*)"Tres");
  second.push((char*)"Cuatro");

  cout << endl << second.getName() << ":" << endl;

  // Pop the lines from the Stack and print them:
  while((s = second.pop()) != 0) {
    cout << s << endl;
  }

  cout << "numStacksCreated: " << Stack::getNumStacksCreated() << endl;
  cout << "publicNumLinksCreated: " << Stack::publicNumLinksCreated << endl;
}
