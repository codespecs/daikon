#include <stdlib.h>
#include <string.h>

char* simple_strcpy (char *dst, const char *src)
{
  char *ret = dst;
  while ((*dst++ = *src++) != '\0');
  return ret;
}

char g_x[] = "my name is";
char g_y[] = "pgbovine";

int g_p = 1;
int g_q = 2;

void testCalloc(int* a, int* b) {}
void testStrCalloc(char* a, char* b) {}
void testMalloc(int* a, int* b) {}

void testGlobalSimpleStrcpy(char* x, char* y) {}
void testDynamicSimpleStrcpy(char* x, char* y) {}

int testTrivial(int a, int b) {return a + b;}

int main() {
  int* l_int_a = (int*)calloc(2, sizeof(int));
  int* l_int_b = (int*)calloc(3, sizeof(int));

  int* l_int_c = (int*)malloc(sizeof(int));
  int* l_int_d = (int*)malloc(sizeof(int));

  char* l_char_a = (char*)calloc(10, 1);
  char* l_char_b = (char*)calloc(10, 1);

  *l_int_c = 101;
  *l_int_d = 202;

  testCalloc(l_int_a, l_int_b);
  testStrCalloc(l_char_a, l_char_b);
  testMalloc(l_int_c, l_int_d);

  // Statically-allocated arrays
  simple_strcpy(g_x, "MY");
  simple_strcpy(g_y, "NAME");
  testGlobalSimpleStrcpy(g_x, g_y);

  // Dynamically-allocated arrays
  simple_strcpy(l_char_a, "hello");
  simple_strcpy(l_char_b, "world");
  testDynamicSimpleStrcpy(l_char_a, l_char_b);

  testTrivial(g_p, 5);
  testTrivial(g_q, 10);

  return 0;
}
