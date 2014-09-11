#include "StackArray.h"
#include <iostream>
using namespace std;

int main() {
  Stack first((char*)"My first stack", 10);

  first.push(101);
  first.push(102);
  first.push(103);
  first.push(104);
  first.push(105);

  // Pop the lines from the Stack and print them:
  cout << first.getName() << ": MAX_ELTS: " << first.getMaxElts();
  cout << ", NUM_STACKS_CREATED: " << first.getNumStacksCreated() << endl;
  int s;

  while((s = first.pop()) != 0) {
    cout << s << endl;
  }

  Stack second((char*)"My second stack", 5);

  second.push(1001);
  second.push(1002);
  second.push(1003);

  cout << second.getName() << ": MAX_ELTS: " << second.getMaxElts();
  cout << ", NUM_STACKS_CREATED: " << first.getNumStacksCreated() << endl;

  // Pop the lines from the Stack and print them:
  while((s = second.pop()) != 0) {
    cout << s << endl;
  }

  return 0;
}
