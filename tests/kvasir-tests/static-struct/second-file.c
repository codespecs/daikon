// Tests Kvasir functionality for only printing
// out file static vars in functions in that
// file and function-specific static vars in
// that function alone

struct desc {
  int *dyn_array;
  int answer;
};

static int second_int = 182;
static int second_int_array[5] = {1, 1, 2, 3, 5};

int* second_examine_int(int* int_array, int* numPtr);

void second_examine(struct desc *p) {
  second_examine_int(second_int_array, &second_int);
}

int* second_examine_int(int* int_array, int* numPtr) {
  return numPtr;
}
