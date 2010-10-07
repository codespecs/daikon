struct desc {
  int *dyn_array;
  int answer;
};

int array1[10];
int array2[20];

static struct desc foo = {array1, 42};
static struct desc bar = {array2, 42};

void examine(struct desc *p) {
  return;
}

int main() {
  int i;
  for (i = 0; i < 10; i++) {
    array1[i] = i;
  }
  for (i = 0; i < 20; i++) {
    array2[i] = 2*i;
  }
  examine(&foo);
  examine(&bar);

  second_examine(&foo);
  return 0;
}
