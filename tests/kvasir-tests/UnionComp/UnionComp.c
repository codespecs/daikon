#include <stdio.h>
#include <stdlib.h>

// This test illustrates an error caused by limitations
// with regards to Kvasir's handling of Unions. Kvasir
// will treat all members of a valid union as valid themselves.
// This becomes problematic in the case of a char*, in which
// it will read from the beginning of the memory region
// pointed to until the first 00 byte. This continuous reading
// of a memory region causes all variable comparabilities to
// be merged.


struct comp_test{
  union ptrs{
    char* name;
    union ptrs *next;
  } comp;
  int not_comp;
  int zero;

} *comp_test_ptr;


void alloc_comp() {
  comp_test_ptr = calloc(1, sizeof(struct comp_test));
  comp_test_ptr->comp.next = &comp_test_ptr->comp;
  comp_test_ptr->not_comp = 0x31323334;
  comp_test_ptr->zero = 0;
}



int main()
{
  int i = 0;

  for(i = 0; i < 256; i++) {
    alloc_comp();
  }

  return 0;
}
