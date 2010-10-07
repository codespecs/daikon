// Adapted for Kvasir regression tests by Philip Guo

//: C04:Stack.cpp {O}
// From Thinking in C++, 2nd Edition
// Available at http://www.BruceEckel.com
// (c) Bruce Eckel 2000
// Copyright notice in Copyright.txt
// Linked list with nesting
#include "Stack.h"
#include <stdlib.h>
#include <string.h>
#include <iostream>
using namespace std;

int Stack::numStacksCreated;
int Stack::publicNumLinksCreated;

int Stack::getNumStacksCreated() {
  return Stack::numStacksCreated;
}

void
Stack::Link::initialize(char* dat, Link* nxt) {
  data = dat;
  next = nxt;
}

Stack::Stack(char* name) {
  myName = strdup(name);
  Stack::numStacksCreated++;
  head = 0;
  numElements = 0;
}

Stack::~Stack() {
  free(myName);
}

char* Stack::getName() {
  cout << "Private stuff: " << privateStuff() << endl;
  return myName;
}

int Stack::privateStuff() {
  return 42;
}

void Stack::push(char* dat) {
  Link* newLink = new Link;
  newLink->initialize(dat, head);
  head = newLink;
  numElements++;
  Stack::publicNumLinksCreated++;
}

char* Stack::peek() {
  return head->data;
}

char* Stack::pop() {
  if(head == 0) return 0;
  char* result = head->data;
  Link* oldHead = head;
  head = head->next;
  delete oldHead;
  numElements--;
  return result;
}
