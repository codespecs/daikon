// Class hierarchy:
//
//         A
//         |
// C   D   E
//  \  |  /
//   \ | /
//     B

#include <stdio.h>

class A {
public:
  int publicIntA;

  void setEverything() {
    publicIntA = 13;
    protectedIntA = 14;
    privateIntA = 15;
  }

protected:
  int protectedIntA;
private:
  int privateIntA;
};

class C {
public:
  int publicIntC;

  void setEverything() {
    publicIntC = 4;
    protectedIntC = 5;
    privateIntC = 6;
  }

protected:
  int protectedIntC;
private:
  int privateIntC;
};

class D {
public:
  int publicIntD;

  void setEverything() {
    publicIntD = 7;
    protectedIntD = 8;
    privateIntD = 9;
  }

protected:
  int protectedIntD;
private:
  int privateIntD;
};

class E : protected A{
public:
  int publicIntE;

  void setEverything() {
    publicIntE = 10;
    protectedIntE = 11;
    privateIntE = 12;

    A::setEverything();
  }

protected:
  int protectedIntE;
private:
  int privateIntE;
};

class B : public C, protected D, private E {
public:
  int publicIntB;

  void setEverything() {
    publicIntB = 1;
    protectedIntB = 2;
    privateIntB = 3;

    C::setEverything();
    D::setEverything();
    E::setEverything();
  }

protected:
  int protectedIntB;
private:
  int privateIntB;
};

B arrayOfBees[10];

int main() {
  A myA;
  B myB;
  C myC;
  D myD;
  E myE;

  myA.setEverything();
  myB.setEverything();
  myC.setEverything();
  myD.setEverything();
  myE.setEverything();

  for (int i = 0; i < 10; i++) {
    arrayOfBees[i].setEverything();
  }

  return 0;
}
