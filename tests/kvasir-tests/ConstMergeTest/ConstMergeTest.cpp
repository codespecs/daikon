#include "file1.h"
#include <cstdio>
extern int var3;

int main(){
  printf("%d %d %d %d\n",var1,var2,var3, getVar());
  return 0;
};
