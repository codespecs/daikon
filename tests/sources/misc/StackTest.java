package misc;

public class StackTest
{
  private static Stack s = new Stack(0);
  private static java.util.Random rnd = new java.util.Random(1000);

  public static void doNew(int size) {
    s = new Stack(size);
    observe();
  }

  public static void push() {
    Object elt = rnd.nextBoolean() ? new Object() : new Integer(0);
    if (!s.isFull()) s.push(elt);
    observe();
  }

  public static void pop() {
    if (!s.isEmpty()) s.pop();
    observe();
  }

  public static void observe() {
    s.isFull();
    s.isEmpty();
  }

  public static void fill(int n) {
    doNew(n);
    for (int i=0; i < n; i++)
      push();
    while( !s.isEmpty( ) )
      pop();

    doNew(n);
    for (int i=0; i <= n / 2; i++) {
      if (!s.isFull()) s.push(s);
      observe();
    }
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
