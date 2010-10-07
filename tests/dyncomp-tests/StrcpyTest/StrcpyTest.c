#include <string.h>

char* simple_strcpy (char *dst, const char *src) {
  char *ret = dst;
  while ((*dst++ = *src++) != '\0');
  return ret;
}

char g_x[] = "AAAA";
char g_y[] = "BBB";

void testSimpleStrcpy(char* x, char* y) {}

int main() {
  simple_strcpy(g_x, "CC");
  simple_strcpy(g_y, "D");

  testSimpleStrcpy(g_x, g_y);

  return 0;
}
