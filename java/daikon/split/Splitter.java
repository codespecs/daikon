package daikon.split;

import daikon.*;

/**
 * A Splitter represents a test that can be used to separate all samples
 * into two parts.  For instance, a Splitter might represent the condition
 * "x > 0".  The Splitter is used to divide a collection of variable values
 * into sub-sets.  Invariant detection can then occur for the two subsets
 * independently.
 */

abstract class Splitter {

  /**
   * Creates a splitter "factory" that should only be used for creating
   * new copies via instantiate(Ppt).
   * There is no need for subclasses to override this.  */
  public Splitter() { }

  /**
   * Creates a valid splitter than can be used for testing the condition
   * via test(ValueTuple).
   */
  abstract public Splitter instantiate(Ppt ppt);

  /**
   * Returns true or false according as whether the values in the specified
   * VarTuple satisfy the condition represented by this Splitter.
   */
  abstract public boolean test(ValueTuple vt);

  // This method could be static; but don't bother making it so.
  /** Returns the condition being tested, as a String. */
  abstract public String condition();

}
