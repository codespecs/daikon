#ifndef DISAMBIG_TEST_H
#define DISAMBIG_TEST_H

typedef struct {
  int* intPtr_P;
  int* intPtr_A;

  char** stringPtr_P;
  char** stringPtr_A;

  char* string_S;
  char* string_C;
  char* string_A;
  char* string_P;

  char char_C;
  char char_I;
} TestBuffer;


char* string_S;
char* string_C;
char* string_A;
char* string_P;

char* globalStringArray[3];

TestBuffer globalTB;

void fooSingle(TestBuffer* tb);
void fooMultiple(TestBuffer* tb, int numElts);

#endif //DISAMBIG_TEST_H
