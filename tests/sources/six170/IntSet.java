package six170;

// Taken from 6.170 Recitation 6, Spring 2001
// Author: Andreas Hoffman (maybe?)

public class IntSet {
  // OVERVIEW:  IntSets are unbounded, mutable sets of integers

  // The rep invariant is
  // c.els != null &&
  // for all integers i . c.els[i] is an Integer &&
  // for all integers i, j . (0 <= i < j < c.els.size) ==>
  //   c.els[i].intValue != c.els[j].intValue)

  private Vector els;

  // constructors
  public IntSet () {
    // @effects:  Initializes this to be empty.
    els = new Vector();
  }

  // mutators
  public void insert (int x) {
    // @modifies: this
    // @effects:  Adds x to the elements of this.
    Integer y = Integer.valueOf(x);
    if (getIndex(y) < 0) els.add(y);
  }

  // observers
  public int size () {
    // @effects:  Returns the cardinality of this.
    return els.size();
  }

  // private methods
  private int getIndex (Integer x) {
    // @effects:  If x is in this, returns index where x appears, else returns -1
    for (int i=0; i < els.size(); i++)
      if (x.equals(els.get(i))) return i;
    return -1;
  }
}
