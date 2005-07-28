package daikon.dcomp;

import static java.lang.System.out;
import java.io.*;

class Test {

  A at;
  int i;
  A[] at_arr;
  double[] d_arr;

  static class A {
    int x;
    A (int val) { x = val;}
    int getx() {
      return x;
    }
    public String toString() { return ("A " + x); }
  }

  public static void main (String[] args) {

    A a1 = new A(1);
    A a2 = new A(2);
    A a3 = new A(3);
    A a4 = new A(4);

    int x = a4.getx();

    t1 (a1, a2, a3, a4);

    System.out.printf ("Value Sets:%n%s%n", TagEntry.dump());

    System.out.printf ("Comparability Info:%n");
    DCRuntime.print_all_comparable();

  }

  public static void t1 (A a1, A a2, A a3, A a4) {

    if (a1 == a2)
      out.printf ("a1 == a2%n");
    else
      out.printf ("a1 != a2%n");

    if (a1 != a2)
      out.printf ("a1 != a2%n");
    else
      out.printf ("a1 == a2%n");

    if (a1 == a1)
      out.printf ("a1 == a1%n");
    else
      out.printf ("a1 != a1%n");

    if (a2 != a2)
      out.printf ("a2 != a2%n");
    else
      out.printf ("a2 == a2%n");

    if (a3 == a3)
      out.printf ("a3 == a3%n");
    if (a4 == a4)
      out.printf ("a4 == a4%n");

  }
}
