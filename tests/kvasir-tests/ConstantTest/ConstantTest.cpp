#include <iostream>

using namespace std;

static const int static_const = 100;
const int global_const = 50;

class A {
public:
  A() {}
  
  int getInt() { return static_const_member;}
  static const int static_const_member = 10;
};

int main(int argc, char** argv) {
  cout << "hi" << endl;
  A a;
  cout << global_const << " - " << static_const << " - " << a.getInt() << endl;
  return 0;
}
