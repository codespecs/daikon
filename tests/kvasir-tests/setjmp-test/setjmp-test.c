#include <stdio.h>
#include <setjmp.h>

static jmp_buf buf;

void func(int a) {
  longjmp(buf, 1);
  printf("func\n");
}

int main() {
  if(!setjmp(buf)) {
    func(0x1);
  }

  return 0;
}
