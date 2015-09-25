#include "stdio.h"

int scalar_common = 1;
int array_common[] = {3,2,1};

int scalar = 0;
int array[] = {1,2,3};

int fun_all_filtered(int x){
  return array_common[2] + 3;
}

int fun_none_filtered(int x){
  return scalar_common - 2;
}

int fun_some_filtered(int x){
  return array[2];
}

int main(){
  printf("%d %d %d\n",fun_all_filtered(2),
        fun_some_filtered(scalar),
        fun_none_filtered(1));
  return 0;
}


