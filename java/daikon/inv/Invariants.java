package daikon.inv;

import daikon.*;
import java.util.*;

// This is essentially a collection of Invariant objects, but with a few
// convenience methods.

// The downside of this extending Vector is that the operations
// return Objects rather than Invariants.
public class Invariants extends Vector {

  // Should this specify a Ppt or PptSlice with which it is associated?
  // Probably.

  public Invariants() {
    super();
  }

  public class LookupIterator implements Iterator {
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
  };


  // These lookup functions should perhaps return Iterators or Enumerations
  // instead.


  // return a list of all the invariants that involve the specified VarInfo
  public Iterator lookup_as_iterator(VarInfo vi) {
    return new LookupIterator(vi);
  }

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
