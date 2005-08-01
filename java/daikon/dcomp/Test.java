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

    System.out.println ("Value Sets:");
    System.out.println (TagEntry.dump());

    System.out.println ("Comparability Info:");
    DCRuntime.print_all_comparable();

  }

  public static void t1 (A a1, A a2, A a3, A a4) {

    if (a1 == a2)
      out.println ("a1 == a2");
    else
      out.println ("a1 != a2");

    if (a1 != a2)
      out.println ("a1 != a2");
    else
      out.println ("a1 == a2");

    if (a1 == a1)
      out.println ("a1 == a1");
    else
      out.println ("a1 != a1");

    if (a2 != a2)
      out.println ("a2 != a2");
    else
      out.println ("a2 == a2");

    if (a3 == a3)
      out.println ("a3 == a3");
    if (a4 == a4)
      out.println ("a4 == a4");

  }
}
