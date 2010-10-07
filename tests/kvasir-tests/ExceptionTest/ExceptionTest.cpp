#include <stdio.h>
#include <stdlib.h>


void thrower(int a) {
  throw 0x33;
}

void function(int a) 
{
  try {
    thrower(0x22);
  } catch(int e) {
    printf("Caught %d\n", e);
  }
}

int main()
{

  function(0x11);
  return 0;
}
