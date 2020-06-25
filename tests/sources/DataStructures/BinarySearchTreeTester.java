package DataStructures;


public class BinarySearchTreeTester {

  public void testMain() {
	main(null);
  }
  private static BinarySearchTree t = new BinarySearchTree();
  private static java.util.Random rnd = new java.util.Random(1000);

  public static void doNew() {
    t = new BinarySearchTree();
    observe();
  }

  public static void observe() {
    t.isEmpty();
    t.findMin();
    t.findMax();
  }

  public static void makeEmpty() {
    t.makeEmpty();
    observe();
  }

  public static void insert(Comparable c) {
    t.insert(c);
    observe();
  }

  public static void remove(Comparable c) {
     t.remove(c);
    observe();
  }

  public static void fillAndRemove(int n) {
    doNew();
    observe();
    MyInteger[] ints = new MyInteger[n];
    t.find(new MyInteger(rnd.nextInt()));
    remove (new MyInteger(rnd.nextInt()));

    for (int i = 0; i < n; i++) {
      ints[i] = new MyInteger(rnd.nextInt());
      insert(ints[i]);
    }

    while (!t.isEmpty()) {
      remove(t.findMax());
    }

    remove(ints[rnd.nextInt(n)]);
    observe();
    makeEmpty();
    remove(new MyInteger(rnd.nextInt()));
    t.find(new MyInteger(rnd.nextInt()));
  }

  public static void randomFillAndRemove(int n) {
    doNew();
    remove(new MyInteger(rnd.nextInt()));
    makeEmpty();
    remove(new MyInteger(rnd.nextInt()));

    for (int i = 0; i < n; i++) {
      if (rnd.nextInt(n) < n/2) {
	insert(new MyInteger(rnd.nextInt()));
	insert(new MyInteger(rnd.nextInt()));
	int j = rnd.nextInt();
	insert(new MyInteger(j));
	t.find(new MyInteger(j));
	try {
	  insert(t.find(new MyInteger(rnd.nextInt())));
	} catch (NullPointerException e) { }
	observe();
      } else {
	remove(new MyInteger(rnd.nextInt()));
      }
    }

    while (!t.isEmpty()) {
      insert(t.findMin());
      remove(t.findMin());
    }
    remove(new MyInteger(rnd.nextInt(n)));
    makeEmpty();
  }

  public static void randomFind(int n) {
    doNew();
    t.find(new MyInteger(rnd.nextInt()));

    MyInteger[] ints = new MyInteger[n];

    for (int i = 0; i < n; i++) {
      ints[i] = new MyInteger(rnd.nextInt());
      insert(ints[i]);
      t.find(ints[i]);
      t.find(new MyInteger(rnd.nextInt()));
    }

    makeEmpty();
    insert(new MyInteger(rnd.nextInt(n)));
  }

  public static void main(String[] args) {
    fillAndRemove(1);
    fillAndRemove(2);
    fillAndRemove(7);
    fillAndRemove(12);
    fillAndRemove(20);
    randomFillAndRemove(4);
    randomFillAndRemove(9);
    randomFillAndRemove(10);
    randomFillAndRemove(12);
    randomFillAndRemove(15);
    randomFind(3);
    randomFind(8);
    randomFind(13);
    randomFind(20);
    randomFind(4);
  }
}
