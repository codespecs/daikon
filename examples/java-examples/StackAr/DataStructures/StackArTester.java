package DataStructures;

import java.util.*;

public class StackArTester
{
  private static Random rnd = new Random(1000);

  private StackAr s = new StackAr(0);

  private StackArTester() { }

  public void doNew(int size)
  {
    s = new StackAr(size);
    observe();
    topAndPop();
  }

  private int phase;
  private final int maxPhase = 5;
  public Object createItem(int i)
  {
    switch (phase) {
    case 0:
      return new MyInteger(i);
    case 1:
      return new Integer(i);
    case 2:
      return new Double(i);
    case 3:
      return new Float(i);
    case 4:
      return new int[i];
    default:
      return new int[i][];
    }
  }

  public void push_noobserve(int i)
  {
    try {
      s.push(createItem(i));
    } catch (Overflow e) { }
  }

  public void push(int i)
  {
    try {
      s.push(createItem(i));
      observe();
    } catch (Overflow e) { }
  }

  public void topAndPop()
  {
    s.topAndPop();
    observe();
  }
  public void top()
  {
    s.top();
    observe();
  }

  public void observe()
  {
    s.isFull();
    s.isEmpty();
    s.top();
  }

  public void repPush(int n)
  {
    for (int i=0; i < n; i++) {
      int x = r.nextInt(1000);
      push(x);
    }
  }

  public void repPush_noobserve(int n)
  {
    for (int i=0; i < n; i++) {
      int x = r.nextInt(1000);
      push_noobserve(x);
    }
  }

  public void repPushOne()
  {
    int x = r.nextInt(1000);
    push(x);
  }

  public void popAll()
  {
    while( !s.isEmpty( ) ) {
      topAndPop();
    }
    top();
    topAndPop();
  }

  public void makeEmpty() {
    observe();
    s.makeEmpty();
    observe();
    top();
    topAndPop();
  }

  public void fillAndEmpty(int n)
  {
    doNew(n);
    repPush(n);
    popAll();
    doNew(n);
    repPush(n);
    makeEmpty();
  }

  public void fillWithSame(int n)
  {
    doNew(n);
    Object elt = createItem(n);
    for (int i=0; i < n; i++) {
      try {
	s.push(elt);
        observe();
      } catch (Overflow e) { }
    }
    topAndPop();
    // popAll();
  }

  public void run_long()
  {
    for (phase = 0; phase <= maxPhase; phase++) {
      fillAndEmpty(0);
      fillAndEmpty(1);
      fillAndEmpty(2);
      fillAndEmpty(5);
      fillAndEmpty(10);
      fillAndEmpty(20);
      fillWithSame(7);
      fillWithSame(22);
    }
    // Avoid invariant about size <= 20
    doNew(22);
    topAndPop();
    repPushOne();
    topAndPop();
    repPush_noobserve(22);
    observe();
    top();
    topAndPop();
    doNew(23);
    // observe();
    repPushOne();
    observe();
    makeEmpty();
  }

  public void run_short()
  {
    phase = 0; fillAndEmpty(0);
    phase = 1; fillAndEmpty(1);
    phase = 2; fillAndEmpty(2);
    phase = 3; fillAndEmpty(5);
    phase = 1; fillAndEmpty(10);
    phase = 2; fillAndEmpty(200);
    phase = 3; fillWithSame(7);
    phase = 4; fillWithSame(22);
  }

  public void run() {
    run_long();
  }

  public static void main(String[] args)
  {
    (new StackArTester()).run();
  }
}
