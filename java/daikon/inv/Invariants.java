package daikon.inv;

import daikon.*;
import java.util.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/


/**
 * This is a collection of Invariant objects, which resizes itself as
 * elements are removed in order to save memory.
 **/
// A downside of this extending ArrayList<Invariant> is that we cannot
// write a type annotation on the type parameter.  It might be better to
// use List<Invariant> and add static helper methods.  Alternately, use
// type qualifier parameters.
public final class Invariants
  extends ArrayList<Invariant>
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public Invariants() {
    super();
  }

  public Invariants(int initialCapacity) {
    super(initialCapacity);
  }

  /**
   * Copy constructor.
   **/
  public Invariants(Invariants arg) {
    super(arg);
  }

  // TODO: Does this method actually make any difference in Daikon's memory use or run-time performance?
  @Override
  public boolean remove(/*@Nullable*/ Object o) {
    boolean result = super.remove(o);
    // Perhaps trim to size.
    // The test I really want is "if size() < capacity()/2".
    // But I don't have a way of determining the capacity.
    // Instead, determine whether the size is a power of 2.
    if (result && isPowerOfTwo(size())) {
      trimToSize();
    }
    return result;
  }

  // Works for non-negative
  /*@Pure*/ private static final boolean isPowerOfTwo(int x) {
    if (x == 0)
      return true;
    // If x is a power of two, then x - 1 has no bits in common with x.
    // OTOH, if x is not a power of two, then x and x - 1 have the same
    // most-significant bit set, so they have at least one bit in common.
    return (x & (x - 1)) == 0;
  }

  /// A non-portable way to obtain the capacity:
  // static int getCapacity(ArrayList<?> l) throws Exception {
  //     Field dataField = ArrayList.class.getDeclaredField("elementData");
  //     dataField.setAccessible(true);
  //     return ((Object[]) dataField.get(l)).length;
  // }

  /// For testing
  // private static final boolean isPowerOfTwoSlow(int x) {
  //   for (int i=0; true; i++) {
  //     int pow = plume.MathMDE.pow(2, i);
  //     if (pow == x)
  //       return true;
  //     if (pow > x)
  //       return false;
  //   }
  // }
  // public static void main(String[] args) {
  //   for (int i=1; i<10000; i++) {
  //     if (isPowerOfTwo(i) != isPowerOfTwoSlow(i)) {
  //       throw new Error("" + i);
  //     }
  //   }
  // }

}
