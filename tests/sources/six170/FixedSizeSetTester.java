package six170;

public class FixedSizeSetTester {  

  private static FixedSizeSet s, t;

  private static void init() {
    s = new FixedSizeSet();
    t = new FixedSizeSet();
    observe();
  }

  public static void observe() {
    for (int i=0; i<8; i++) {
      s.contains(i);
    }
  }
  private static java.util.Random rnd = new java.util.Random(1000);
  public static int num() { return (rnd.nextInt() & 7); }

  public static void add(int n) { s.add(n); observe(); s.add(n); observe(); }
  public static void remove(int n) { s.remove(n); observe(); s.remove(n); observe(); }

  public static void main(String[] args) {
    init();
    add(0); remove(0);
    for (int x=0; x<200; x++) {
      add(num());
      add(num());
      remove(num()); 
    }    
    for (int x=0; x<50; x++) {
      init();
      for (int y=0; y < num() * 2; y++) {
	s.add(num());
	t.add(num());
      }
      if ((num() & 1) == 0) {
	s.intersect(t);
      } else {
	s.union(t);
      }
    }
  }
}
