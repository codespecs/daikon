package daikon.inv;

import daikon.*;
import java.util.*;

// The downside of this extending Vector is that the operations
// return Objects rather than Invariants.

/**
 * This is essentially a collection of Invariant objects, but with a few
 * convenience methods.
 **/
public final class Invariants
  extends ArrayList
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

  /** Implementation of lookup_as_iterator. **/
  public final class LookupIterator implements Iterator {
    Iterator invs_enum = iterator();
    Object next = null;     // the next element that will be returned
    boolean next_valid = false;

    VarInfo vi;

    public LookupIterator(VarInfo vi) { this.vi = vi; }

    public boolean hasNext() {
      while ((!next_valid) && invs_enum.hasNext()) {
        Invariant inv = (Invariant) invs_enum.next();
        if (inv.usesVar(vi)) {
          next = inv;
          next_valid = true;
        }
      }
      return next_valid;
    }

    public Object next() {
      if (!hasNext()) {
        throw new NoSuchElementException("Invariants Enumeration");
      }
      next_valid = false;
      return next;
    }

    public void remove() { throw new UnsupportedOperationException(); }
  }


  // Return an Iterator of all the invariants that involve the specified
  // VarInfo.
  public Iterator lookup_as_iterator(VarInfo vi) {
    return new LookupIterator(vi);
  }

  // Return a list of all the invariants that involve the specified
  // VarInfo.
  public Vector lookup_as_vector(VarInfo vi) {
    Vector result = new Vector();
    for (Iterator i = iterator(); i.hasNext() ; ) {
      Invariant inv = (Invariant) i.next();
      if (inv.usesVar(vi))
        result.add(inv);
    }
    return result;
  }

  // return a list of all the invariants that involve the specified VarInfos
  public Vector lookup(VarInfo vi1, VarInfo vi2) {
    Vector result = new Vector();
    for (Iterator i = iterator(); i.hasNext() ; ) {
      Invariant inv = (Invariant) i.next();
      if (inv.usesVar(vi1) && inv.usesVar(vi2))
        result.add(inv);
    }
    return result;
  }

  // return a list of all the invariants that involve the specified VarInfos
  public Vector lookup(VarInfo vi1, VarInfo vi2, VarInfo vi3) {
    Vector result = new Vector();
    for (Iterator i = iterator(); i.hasNext() ; ) {
      Invariant inv = (Invariant) i.next();
      if (inv.usesVar(vi1) && inv.usesVar(vi2) && inv.usesVar(vi3))
        result.add(inv);
    }
    return result;
  }

  // Override superclass implementation
  public boolean remove(Object o) {
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

  // Remove all the invariants in toRemove. This is faster than
  // repeatedly calling remove(), if toRemove is long.
  public int removeMany(List toRemove) {
    HashSet removeSet = new HashSet(toRemove);
    ArrayList copy = new ArrayList();
    Iterator it = this.iterator();
    while (it.hasNext()) {
      Object inv = it.next();
      if (!removeSet.contains(inv))
        copy.add(inv);
    }
    int numRemoved = size() - copy.size();
    clear();
    addAll(copy);
    return numRemoved;
  }

  // Works for non-negative
  private static final boolean isPowerOfTwo(int x) {
    if (x == 0)
      return true;
    // If x is a power of two, then x - 1 has no bits in common with x
    // OTOH, if x is not a power of two, then x and x - 1 have the same
    // most-significant bit set, so they have at least one bit in common.
    return (x & (x - 1)) == 0;
  }

  /// For testing
  // private static final boolean isPowerOfTwoSlow(int x) {
  //   for (int i=0; true; i++) {
  //     int pow = utilMDE.MathMDE.pow(2, i);
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
