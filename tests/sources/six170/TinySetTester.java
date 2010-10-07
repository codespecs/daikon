package six170;

import java.util.Random;

// This class is identical to TinySetTester.
// If you change one, be sure to change the other.
public class TinySetTester {
  
  public static void main(String[] args) {
    (new TinySetTester()).run();
  }

  public void observe(TinySet s) {
    for (int i=0; i<8; i++) {
      s.contains(i);
    }
  }

  private static Random rnd = new Random(1000);
  public int num() {
    return (rnd.nextInt() & 7);
  }

  public TinySet make() {
    TinySet x = new TinySet();
    observe(x);
    return x;
  }

  public void run() {
    TinySet t, s = make();
    observe(s);

    for (int x=0; x<100; x++) {
      int n = num();
      s.add(n);
      observe(s);
      s.remove(n);
      observe(s);
    }

    for (int x=0; x<100; x++) {
      s.add(num());
      observe(s);
      s.remove(num());
      observe(s);
    }

    for (int x=0; x<50; x++) {
      int n = num();
      s.add(n);
      observe(s);
      s.add(n);
      observe(s);
    }
    
    for (int x=0; x<50; x++) {
      t = make();
      s = make();
      for (int y=0; y < (num() & 7); y++) {
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
