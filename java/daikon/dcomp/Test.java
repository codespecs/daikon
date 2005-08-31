package daikon.dcomp;

import static java.lang.System.out;
import java.io.*;

class Test {

  A at;
  static int i;
  static int j;
  static A sa1 = new A ("sa1");
  static A sa2 = new A ("sa2");
  // A[] at_arr;
  // double[] d_arr;

  static class A {
    String id;
    int x;
    int y;

    public A(String id) {
      this.id = id;
      x = 1;
      y = 2;
    }
    public void add() {
      x = x + y;
      if (false)
        throw new RuntimeException ("exception in add");
    }
    public void add(int val) {
      x += val;
    }
    public void tta () {
      add (y);
    }
    public String toString() { return ("A " + id); }
  }

  public static class C {

    String cid;

    C (String id) {
      cid = id;
    }

    public String toString() {
      return cid;
    }

  }

  public static class B {

    A[] a1a;
    A[] a2a;
    A a1;
    int ii = 2;
    int jj = 1;

    B() {
      a1a = new A[] {new A("a1a-0"), new A("a1a-1"), new A("a1a-2")};
      a2a = new A[] {new A("a2a-0"), new A("a2a-1"), new A("a2a-2"),
                     new A("A2a-3")};
    }

    void ecomp() {

      if (a1a[ii] == a2a[jj])
        System.out.println ("a1a[2] == a2a[1]");
      else
        System.out.println ("a1a[2] != a2a[1]");
    }

    void p (A aval) {
      a1a[2].add();
    }

    void comp() {
      if (a1a == a2a)
        System.out.println ("a1a == a2a");
      else
        System.out.println ("a1a != a2a");

    }
  }

  public static void main (String[] args) throws Exception {

    test();

  }

  public static void test() {

    B b1 = new B();
    b1.ecomp();
    b1.p (new A ("a0"));
    b1.comp();

    A a1 = new A("a1");
    a1.add (i);
    a1.add (j);
    a1.add();
    a1.tta();

    if (true) {
      A a2 = new A("a2");
      A a3 = new A("a3");
      A a4 = new A("a4");

      t1 (a1, a2, a3, a4);
    }

    if (sa1 == sa2)
      out.println ("sa1 == sa2");
    else
      out.println ("sa1 != sa2");

    double_check (1.2, 56, 1);


  }

  public static double double_check (double d1, Integer wrapper, int i1) {

    double loc1 = 22.4;
    double loc2 = loc1 + 14.6;

    d1 += loc2;
    i1 += loc1;

    return ((double) i1);
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
