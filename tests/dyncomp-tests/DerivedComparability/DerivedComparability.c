// However, if two formal parameters become comparable solely because
// their actual parameters in one call are comparable (derived
// comparability), then all sets of actual parameters do NOT
// automatically become comparable.

void m(int x, int y) {
  return;
}

void n(int x, int y) {
  return;
}

int main() {
  int a, b = 2, c = 3, d = 4;

  a = b;

  m(a, b);
  m(c, d);

  n(c, d);

  return 0;
}
