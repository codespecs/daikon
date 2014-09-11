// Adapted for Kvasir regression tests by Philip Guo

// This is an INLINE version where all member function declarations
// are located WITHIN the class body instead of outside in a separate
// file

//: C04:Stack.h
// From Thinking in C++, 2nd Edition
// Available at http://www.BruceEckel.com
// (c) Bruce Eckel 2000
// Copyright notice in Copyright.txt
// Nested struct in linked list
#ifndef STACK_H
#define STACK_H

#include <stdlib.h>
#include <iostream>
#include <string.h>
using namespace std;

class Stack {
 private:
  static int numStacksCreated;

 public:
  static int publicNumLinksCreated;

  void push(char* dat) {
    Link* newLink = new Link;
    newLink->initialize(dat, head);
    head = newLink;
    numElements++;
    publicNumLinksCreated++;
  }

  char* peek() {
    return head->data;
  }

  char* pop() {
    if(head == 0) return 0;
    char* result = head->data;
    Link* oldHead = head;
    head = head->next;
    delete oldHead;
    numElements--;
    return result;
  }

  char* getName() {
    cout << "Private stuff: " << privateStuff() << endl;
    return myName;
  }

  Stack(char* name) {
    myName = strdup(name);
    numStacksCreated++;
    head = 0;
    numElements = 0;
  }

  static int getNumStacksCreated() {
    return numStacksCreated;
  }

  ~Stack() {
    free(myName);
  }

 private:

  int numElements;
  char* myName;

  int privateStuff() {
    return 42;
  }

  Stack(char* name, int x) {
      throw "should never get here";
  }

  struct Link {
    char* data;
    Link* next;
    void initialize(char* dat, Link* nxt) {
      data = dat;
      next = nxt;
    }
  }* head;

};
#endif // STACK_H ///:~
