package daikon.inv;

import daikon.*;

// An interface satisfied by OneOfScalar and OneOfSequence.
public interface OneOf {
  /** The number of elements in the OneOf invariant. */
  public int num_elts();

  /**
   * The single value represented by the OneOf invariant.
   * Throws an error if not exactly one value is represented by this.
   */
  public Object elt();

  public VarInfo var();
}
