package utilMDE;

/**
 * Simple pair class.  Moved from OrderedPairIterator to its own
 * class, so it can be used in more places.
 */
public class Pair {
  public Object a;
  public Object b;

  public Pair(Object a, Object b) {
    this.a = a;
    this.b = b;
  }

  public String toString() {
    return "<" + String.valueOf(a) + "," + String.valueOf(b) + ">";
  }
}

