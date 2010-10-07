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
  Stack first("My first stack");

  first.push("First line");
  first.push("Second line");
  first.push("Third line");
  first.push("Fourth line");
  first.push("Fifth line");

  // Pop the lines from the Stack and print them:
  char* s;

  cout << first.getName() << ":" << endl;

  while((s = first.pop()) != 0) {
    cout << s << endl;
  }

  cout << "numStacksCreated: " << Stack::getNumStacksCreated() << endl;
  cout << "publicNumLinksCreated: " << Stack::publicNumLinksCreated << endl;

  Stack second("My second stack");

  second.push("Uno");
  second.push("Dos");
  second.push("Tres");
  second.push("Cuatro");

  cout << endl << second.getName() << ":" << endl;

  // Pop the lines from the Stack and print them:
  while((s = second.pop()) != 0) {
    cout << s << endl;
  }

  cout << "numStacksCreated: " << Stack::getNumStacksCreated() << endl;
  cout << "publicNumLinksCreated: " << Stack::publicNumLinksCreated << endl;
}
