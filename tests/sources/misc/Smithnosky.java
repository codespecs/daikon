package misc;

import java.util.Vector;

// Daikon test by Jesse Smithnosky
public class Smithnosky {

  private int a;
  private int b;
  private int c;
  private int aXb;
  private int aXc;
  private int size;
  private Vector s;

  public Smithnosky(int a, int b, int c) {
    a = a;
    b = b;
    c = c;
    aXb = a*b;
    aXc = a*c;
    s = new Vector();
    size = 0;
  }

  public void incA() {
    a = a + 1;
    aXb = a*b;
  }

  public void incB() {
    b++;
    aXb = a*b;
  }

  public void incC() {
    c++;
    aXc = a*c;
  }

  public void add(Object o) {
    s.add(o);
    size++;
  }

  public static void main(String args[]) {
    Smithnosky myTest = new Smithnosky(2,3,4);
    myTest.incA();
    myTest.incB();
    myTest.incC();
    myTest.incA();
    myTest.incB();
    myTest.incC();
    myTest.incA();
    myTest.incB();
    myTest.incC();
    myTest.add("A");
    myTest.add("A");
  }
}
