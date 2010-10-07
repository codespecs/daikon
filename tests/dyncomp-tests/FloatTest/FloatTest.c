double returnDoubleSum(double a, double b)
{
  return a + b;
}

double returnInvalidDouble(double a, double b)
{
  double c;
  return c;
}

int returnIntSum(double a, double b)
{
  return a + b;
}

int returnInvalidInt(double a, double b)
{
  int c;
  return c;
}

int main() {
  double x = 100.1, y = 239.3;

  returnInvalidDouble(3.14159, 2.7181);
  returnInvalidDouble(x, y);

  returnDoubleSum(1.2345, 4.321);
  returnInvalidInt(1.111, 2.222);

  returnIntSum(10.2, 6.7);

  return 0;
}
