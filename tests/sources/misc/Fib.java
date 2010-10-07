package misc;

// Written by Toh, March 2002

public class Fib
{
  public static final int STEPS = 20;

  public static int a, b, c;

  public static void main(String[] args)
  {
    a = 0;
    b = c = 1;
    for (int i = 0; i < STEPS; i++) {
      increment();
    }
  }

  public static void increment()
  {
    a = b;
    b = c;
    c = a + b;
  }

}
