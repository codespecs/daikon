package misc;
import java.util.*;

/**
 * Suppression testing.  This tests the suppressors:
 * <br>
 *   0<=i<=j  ==>  b[i] in b[0..j]         
 *
 */


class Suppress02 extends Object {
  public int[] theArray;
  public int i, j;

  public static void main(String[] args) {
    Suppress02 p = new Suppress02();
    
    for (int n = 0; n < 250; n++) {
      p.f(n);
    }
  }

  public void f(int n) {
    i = n % 50;
    j = i + n % 48;
  }


  public Suppress02 () {
    theArray = new int[1000];
    for (i = 0; i < theArray.length; i++ ) {
      theArray[i] = i * 30293 % 392;
    }
    this.i = 0;
    this.j = 5;
  }

}

