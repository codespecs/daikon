package DataStructures;

public class QueueArTester {
  private static QueueAr q = new QueueAr(1);
  private static java.util.Random rnd = new java.util.Random(1000);

  public static void doNew(int size) {
    q = new QueueAr(size);
    observe();
  }

  public static void observe() {
    q.isEmpty();
    q.isFull();
    q.getFront();
  }

  public static void makeEmpty() {
    q.makeEmpty();
    observe();
  }

  public static void enqdeq() {
    enqueue(); dequeue();
  }

  public static void dequeue() {
    q.dequeue();
    observe();
  }

  public static void enqueue() {
    try {
      q.enqueue(rnd.nextBoolean() ? new Object(): new MyInteger(0));
    } catch (Exception e) { }
    observe();
  }

  public static void fillAndEmpty(int n) {
    doNew(n);
    for (int i=0; i < n; i++) enqueue();
    q.dequeueAll();
//    while(!q.isEmpty()) dequeue();
  }

  public static void walkAround(int n) {
    doNew(n);
    for (int i=0; i < n/2; i++) enqueue();
    for (int i=0; i < 3*n; i++) enqdeq();
    while(!q.isEmpty()) dequeue();
  }

  public static void variedMakeEmpty(int n) {
    doNew(n);
    for (int i=0; i < n; i++) enqueue();
    makeEmpty();
    for (int z=0; z < n; z++) {
      doNew(n);
      for (int i=0; i < z; i++) enqueue();
      for (int i=0; i < 3*z; i++) enqdeq();
      makeEmpty();
    }
  }

  public static void main(String[] args) {
    fillAndEmpty(1);
    fillAndEmpty(2);
    fillAndEmpty(10);
    fillAndEmpty(30);
    walkAround(5);
    walkAround(10);
    walkAround(30);
    variedMakeEmpty(1);
    variedMakeEmpty(2);
    variedMakeEmpty(10);
    variedMakeEmpty(12);
  }

}
