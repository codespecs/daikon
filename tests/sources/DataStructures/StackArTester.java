package DataStructures;

import java.util.*;

public class StackArTester
{
  private static StackAr s = new StackAr(0);
  private static java.util.Random rnd = new java.util.Random(1000);

  public static void doNew(int size) {
    s = new StackAr(size); s.makeEmpty();
    observe();
    topOrPop();
  }

  public static void push() {
    try {
      s.push(rnd.nextBoolean() ? new Object() : new MyInteger(0));
    } catch (Overflow e) { }
    observe();
  }

  public static void topOrPop() {
    if (s.isEmpty() || rnd.nextBoolean()) s.topAndPop();
    else try { s.pop(); }
    catch (Underflow e) { }
    observe();
  }

  public static void observe() {
    s.isFull();
    s.isEmpty();
    s.top();
  }

  public static void fill(int n) {
    doNew(n);
    for (int i=0; i < n; i++)
      push();
    if (rnd.nextBoolean())
      s.makeEmpty();
    while( !s.isEmpty( ) )
      topOrPop();
    s.makeEmpty();
    observe();

    doNew(n);
    for (int i=0; i <= n / 2; i++) {
      try {
	s.push(s);
        observe();
      } catch (Overflow e) { }
    }
    s.makeEmpty();
  }

  public static void main(String[] args) {
    for (int i=0; i < 4; i++) {
      doNew(0);
      fill(i);
      fill(10);
      fill(20);
    }
  }

}
