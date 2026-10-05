import java.util.ArrayList;
import java.util.List;

/**
 * A program whose behavior changes if any method other than those it calls itself is invoked on
 * its objects. Used to test Chicory's {@code --no-method-calls} command-line option.
 */
public class NoMethodCalls {

  /** The number of times that {@link CountingList#toArray()} has been called. */
  static int toArrayCalls = 0;

  /** A list whose {@code toArray()} method has a side effect. */
  @SuppressWarnings("serial")
  static class CountingList extends ArrayList<Object> {
    @Override
    public Object[] toArray() {
      toArrayCalls++;
      return super.toArray();
    }
  }

  public static void main(String[] args) {
    List<Object> base = new ArrayList<>();
    // Calling a method of `subList` after modifying `base` throws ConcurrentModificationException.
    List<Object> stale = base.subList(0, 0);
    base.add("");
    for (int i = 0; i < 10; i++) {
      CountingList cl = new CountingList();
      for (int j = 0; j < i; j++) {
        cl.add(j);
      }
      observe(cl, stale, i);
    }
    if (toArrayCalls != 0) {
      throw new Error("toArray() was called " + toArrayCalls + " times");
    }
  }

  public static int observe(List<Object> list, List<Object> stale, int x) {
    return x + 1;
  }
}
