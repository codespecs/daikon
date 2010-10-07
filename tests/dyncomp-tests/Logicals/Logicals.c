// Logical AND and OR do not imply interaction

void logicalAnd8(char a, char b) {}
void logicalOr8(char a, char b) {}

void logicalAnd16(short a, short b) {}
void logicalOr16(short a, short b) {}

void logicalAnd32(int a, int b) {}
void logicalOr32(int a, int b) {}

void logicalAnd64(long long int a, long long int b) {}
void logicalOr64(long long int a, long long int b) {}

int main() {
  char a8 = 'a';
  char b8 = 'b';

  short a16 = 5000;
  short b16 = 10000;

  int a32 = 123456789;
  int b32 = 987654321;

  long long int a64 = 0x0f0f0f0fffffffff;
  long long int b64 = 0x0f0f0f0fffffffff;

  if (a8 && b8)
    logicalAnd8(a8, b8);

  if (a8 || b8)
    logicalOr8(a8, b8);


  if (a16 && b16)
    logicalAnd16(a16, b16);

  if (a16 || b16)
    logicalOr16(a16, b16);


  if (a32 && b32)
    logicalAnd32(a32, b32);

  if (a32 || b32)
    logicalOr32(a32, b32);


  if (a64 && b64)
    logicalAnd64(a64, b64);

  if (a64 || b64)
    logicalOr64(a64, b64);


  return 0;
}
