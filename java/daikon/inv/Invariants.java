package daikon.inv;

import daikon.*;
import java.util.*;

// The downside of this extending Vector is that the operations
// return Objects rather than Invariants.

/**
 * This is essentially a collection of Invariant objects, but with a few
 * convenience methods.
 **/
public final class Invariants extends Vector {

  public Invariants() {
    super();
  }

  /** Implementation of lookup_as_iterator. **/
  public final class LookupIterator implements Iterator {
    Enumeration invs_enum = elements();
    Object next = null;     // the next element that will be returned
    boolean next_valid = false;

    VarInfo vi;

    public LookupIterator(VarInfo vi_) { vi = vi_; }

    public boolean hasNext() {
      while ((!next_valid) && invs_enum.hasMoreElements()) {
        Invariant inv = (Invariant) invs_enum.nextElement();
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

  // Return an Iterator of all the invariants that involve the specified
  // VarInfo.
  public Vector lookup_as_vector(VarInfo vi) {
    Vector result = new Vector();
    for (Enumeration e = elements(); e.hasMoreElements() ; ) {
      Invariant inv = (Invariant) e.nextElement();
      if (inv.usesVar(vi))
        result.add(inv);
    }
    return result;
  }

  // return a list of all the invariants that involve the specified VarInfos
  public Vector lookup(VarInfo vi1, VarInfo vi2) {
    Vector result = new Vector();
    for (Enumeration e = elements(); e.hasMoreElements() ; ) {
      Invariant inv = (Invariant) e.nextElement();
      if (inv.usesVar(vi1) && inv.usesVar(vi2))
        result.add(inv);
    }
    return result;
  }

  // return a list of all the invariants that involve the specified VarInfos
  public Vector lookup(VarInfo vi1, VarInfo vi2, VarInfo vi3) {
    Vector result = new Vector();
    for (Enumeration e = elements(); e.hasMoreElements() ; ) {
      Invariant inv = (Invariant) e.nextElement();
      if (inv.usesVar(vi1) && inv.usesVar(vi2) && inv.usesVar(vi3))
        result.add(inv);
    }
    return result;
  }

}
