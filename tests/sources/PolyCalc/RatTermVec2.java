package PolyCalc;

/** RatTermVec is a mutable, growable array of RatElts.  Such arrays
    are commonly referred to as <i>vectors</i>.	 One can access
    RatElts in a RatTermVec using an integer index (starting at
    zero).  The size of a RatTermVec grows (or shrinks) as needed to
    accommodate the addition and removal of RatElts.
    <p>
    The current state of a RatTermVec can be notated as a sequence:
    [Ta, Tb, ...].  Examples of RatTermVecs include [] (an empty
    vector), [Ta], [Ta, Tb], and [Ta, Tb, Ta], where Ta and Tb are
    RatTerm objects.
*/
public class RatTermVec2 {
  // Definitions:
  // For a RatTermVec r, let T(r,i) be "r.elts.get(i)"

  // AF(r) = forall i=0 to r.elts.size()-1, [T(r,0), T(r,1), ..., T(r,i), ...]

  // RI(r) = r.elts != null && forall i=0 to r.elts.size()-1, T(r,i) is-a Term

  private RatTerm[] elts = new RatTerm[10];
  private int used = 0;

  /** @effects: constructs a new empty RatTermVec, []. */
  public RatTermVec2() {
  }

  private RatTermVec2(RatTermVec2 other) {
    System.arraycopy(other.elts, 0, elts, 0, other.used);
    used = other.used;
  }

  /** @return the size of this RatTermVec. */
  public int size() {
    return used;
  }

  /** Indexing operation.
      @requires: 0 <= index < this.size()
      @return the RatTerm at the specified index.
      <br>
      e.g. Given a RatTermVec v = [t2, t3, t4], the expression
      "v.get(1)" will return the RatTerm t3.
  */
  public RatTerm get(int index) {
    return elts[index];
  }

  /** Appending operation.
      @requires: t != null
      @modifies: this
      @effects: Adds the specified RatTerm, 't', to the end of this
      vector, increasing the vector's size by one.
      <br>
      e.g. Given a RatTermVec v = [t2, t3, t4], the statement 
      "v.addElement(t3);" will make v_post = [t2, t3, t4, t3].
  */
  public void addElement(RatTerm t) {
    elts[used++] = t;
  }

  /** Insertion operation.
      @requires: t != null && 0 <= index <= this.size()
      @modifies: this
      @effects: Inserts 't' as a component in this RatTermVec at the
      specified index. Each component in this vector with an index
      greater or equal to the specified index is shifted upward to
      have an index one greater than the value it had previously.
      The size of this vector is increased by 1.
      <br>
      e.g. Given a RatTermVec v = [t2, t3, t4], the statement
      "v.insert(t5, 1);" will make v_post = [t2, t5, t3, t4].
  */
  public void insert(RatTerm t, int index) {
    System.arraycopy(elts, index, elts, index+1, (used - index));
    elts[index] = t;
    used++;
  }

  /** Deletion operation.
      @requires: 0 <= index < this.size()
      @modifies: this
      @effects: Deletes the RatTerm at the specified index. Each
      RatTerm in this vector with an index greater or equal to the
      specified index is shifted downward to have an index one
      smaller than the value it had previously. The size of this
      vector is decreased by 1.
      <br>
      e.g. Given a RatTermVec v = [t2, t3, t4], the statement
      "v.remove(1);" will make v_post = [t2, t4].
  */
  public void remove(int index) {
    System.arraycopy(elts, index+1, elts, index, (used - (index+1)));
    used--;
    elts[used] = null;
  }

  /** Replacement operation.
      @requires: t != null && 0 < index < this.size()
      @modifies: this
      @effects: Sets the RatTerm at the 'index' of this vector to be
      't'. The previous RatTerm at 'index' is discarded.
      <br>
      e.g. Given a RatTermVec v = [t2, t3, t4], the statement
      "v.set(t5, 1);" will make v_post = [t2, t5, t4].
  */
  public void set(RatTerm t, int index) {
    elts[index] = t;
  }

  /** Copy operation.
      @return a new RatTermVec whose initial state matches that of
      this RatTermVec.  Changes made to the state of the returned vector
      will NOT be reflected in this vector, and vice versa.  (Also recall
      that RatTerm objects are immutable.)
  */
  public RatTermVec2 copy() {
    return new RatTermVec2(this);
  }

  /** @return implementation specific debugging string. */
  public String printDebug() {
    return "RatTermVec<elts:"+this.elts+">";
  }

  public String toString() { return printDebug(); }
}
