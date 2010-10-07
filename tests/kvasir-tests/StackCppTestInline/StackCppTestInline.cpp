// Adapted for Kvasir regression tests by Philip Guo

// This version uses a Stack class whose member functions
// are declared inline within Stack.h instead of outside
// in a separate file (Stack.cpp)

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

int Stack::numStacksCreated;
int Stack::publicNumLinksCreated;

int main() {
  Stack first("My first stack");

  first.push("First line");
  first.push("Second line");
  first.push("Third line");
  first.push("Fourth line");
  first.push("Fifth line");

  // THIS IS SUPER WEIRD! IF YOU COMMENT OUT THIS NEXT LINE AND DON'T
  // USE peek() AT ALL IN YOUR PROGRAM, AN ENTRY FOR IT WILL NOT
  // APPEAR ANYWHERE, NOT EVEN IN THE SYMBOL TABLE, SO FOR ALL INTENTS
  // AND PURPOSES, IT WON'T EXIST.  Interesting, huh?  But if it's
  // declared outside the class body, then it will exist
  // unconditionally.
  first.peek();

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
