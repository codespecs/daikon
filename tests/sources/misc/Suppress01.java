package misc;

/**
 * Suppression testing.  This tests the suppressors:
 * <br>
 * x \subset y => x[a] \in y
 * x[] == y[] => x subset y (and vice versa)
 * x[] == y[] => x subseq y (and vice versa)
 * x is a constant => all functionBinary invariants with x are uninteresting.
 */


class Suppress01 extends Object {
  public int[] theArray;
  public int[] theArray2;
  public int b;

  public static void main(String[] args) {
    Suppress01 p = new Suppress01();
    
    for (int i = 0; i < 25; i++) {
      p.f();
    }
  }

  public void f() {
    theArray = new int[theArray.length + 1];
    theArray2 = new int[theArray.length + 2];
    int i;
    for (i = 0; i < theArray.length-1; i++ ) {
      theArray[i] = i;
      theArray2[i] = i;
    }
    theArray2[i] = i;
  }


  public Suppress01 () {
    theArray = new int[1];
    theArray[0] = 0;
    theArray2 = new int[] {0, 1};
    b = 0;
  }

}

