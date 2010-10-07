// Adapted for Kvasir regression tests by Philip Guo

//: C04:Stack.h
// From Thinking in C++, 2nd Edition
// Available at http://www.BruceEckel.com
// (c) Bruce Eckel 2000
// Copyright notice in Copyright.txt
// Nested struct in linked list
#ifndef STACK_H
#define STACK_H

class Stack {
 public:
  void push(char* dat);
  char* peek();
  char* pop();
  char* getName();
  Stack(char* name);
  static int getNumStacksCreated();
  static int publicNumLinksCreated;
  ~Stack();

 private:
  int numElements;
  char* myName;
  static int numStacksCreated;
  int privateStuff();

  struct Link {
    char* data;
    Link* next;
    void initialize(char* dat, Link* nxt);
  }* head;
};
#endif // STACK_H ///:~
