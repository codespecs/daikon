package daikon.split;

import daikon.*;
import java.io.Serializable;

/**
 * A Splitter represents a test that can be used to separate all samples
 * into two parts.  For instance, a Splitter might represent the condition
 * "x > 0".  The Splitter is used to divide a collection of variable values
 * into sub-sets.  Invariant detection can then occur for the two subsets
 * independently.
 */

public abstract class Splitter
  implements Serializable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /**
   * Creates a splitter "factory" that should only be used for creating new
   * copies via instantiate(Ppt).  (That is, the result of "new Splitter()"
   * should not do any splitting itself.)  There is no need for subclasses
   * to override this (but most will have to, since they will add
   * their own constructors as well).
   **/
  public Splitter() { }

  /**
   * Creates a valid splitter than can be used for testing the condition
   * via test(ValueTuple).  The implementation should always set
   * the "instantiated" protected field to true, if that field is present
   * in the Splitter class.
   */
  public abstract Splitter instantiate(Ppt ppt);

  // For debugging.  Commented out to avoid need to change serialVersionUID.
  // protected boolean instantiated = false;
  // /**
  //  * Returns true for an instantiated (non-"factory") splitter.
  //  * Clients also need to check valid().
  //  **/
  // public boolean instantiated() {
  //   return instantiated;
  // }

  /**
   * Returns true or false according to whether this was instantiated
   * correctly and test(ValueTuple) can be called without error.
   * An alternate design would have instantiate(Ppt) check this,
   * but it's a bit easier on implementers of subclasses of Splitter
   * for the work to be done (in just one place) by the caller.
   */
  public abstract boolean valid();

  /**
   * Returns true or false according to whether the values in the specified
   * ValueTuple satisfy the condition represented by this Splitter.
   */
  public abstract boolean test(ValueTuple vt);

  // This method could be static; but don't bother making it so.
  /** Returns the condition being tested, as a String. */
  public abstract String condition();

}
